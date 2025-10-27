# =========================
# Stage 1: builder
# =========================
FROM ubuntu:22.04 AS builder

ARG EMACS_VERSION=30.1
ARG MU_VERSION=1.12.9
ENV DEBIAN_FRONTEND=noninteractive TZ=UTC
ENV HOME=/emacs

# Make sure we use the matching compiler that owns the JIT
ENV CC=gcc-12 CXX=g++-12

# Base tools + Emacs build deps (GTK/WebKitGTK for xwidgets, libgccjit for native-comp)
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates curl git build-essential pkg-config \
    autoconf automake texinfo xz-utils file \
    libgtk-3-dev libwebkit2gtk-4.0-dev libharfbuzz-dev \
    gcc-12 g++-12 libgccjit-12-dev libjansson-dev libgnutls28-dev libxml2-dev libsqlite3-dev \
    libxpm-dev libgif-dev libjpeg-dev libpng-dev libtiff-dev librsvg2-dev \
    libtree-sitter-dev \
    libncurses-dev libtinfo-dev \
    git zip unzip curl wget \
    # vterm build dependencies
    cmake libtool-bin libvterm-dev \
 && rm -rf /var/lib/apt/lists/*

# ---- Build Emacs (native-comp + JSON + GnuTLS) ----
WORKDIR /tmp/emacs
RUN curl -fsSLO https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz \
 && tar -xf emacs-${EMACS_VERSION}.tar.xz \
 && cd emacs-${EMACS_VERSION} \
 && ./configure \
      --prefix=/opt/emacs \
      --with-native-compilation \
      --with-x-toolkit=gtk3 \
      --with-gnutls \
      --with-modules \
      --without-selinux --without-gpm --without-xaw3d --without-pgtk \
 && make -j"$(nproc)" NATIVE_FULL_AOT=1 \
 && make install \
 && /opt/emacs/bin/emacs --batch --eval "(princ (emacs-version))"

# ---- Your ~/.emacs.d (literate) ----
# Provide these files in your build context:
#   readme.org  early-init.el  (optional: bootstrap.el if you have one)
RUN mkdir -p /emacs/.config/emacs
COPY --chmod=0755 early-init.el   /emacs/.config/emacs/early-init.el
COPY --chmod=0755 init.el         /emacs/.config/emacs/init.el
COPY --chmod=0755 readme.org      /emacs/.config/emacs/readme.org
COPY --chmod=0755 readme.el       /emacs/.config/emacs/readme.el
COPY --chmod=0755 entrypoint.sh   /emacs/.config/emacs/entrypoint.sh
COPY --chmod=0755 build-packages.el /emacs/.config/emacs/build-packages.el

# If you have a bootstrap file (elpaca setup), uncomment the next line
# COPY --chmod=0755 bootstrap.el    /emacs/.config/emacs/bootstrap.el

# ---- Tangle config files and install packages ----
RUN mkdir -p /emacs/.config/emacs/.local \
 && mkdir -p /emacs/Documents/org/jira \
 && mkdir -p /emacs/Documents/org/calendars \
 && mkdir -p /emacs/Documents/org/roam \
 && touch /emacs/Documents/org/jira/.keep \
 && touch /emacs/Documents/org/calendars/.keep \
 && touch /emacs/Documents/org/roam/.keep \
 && ls -la /emacs/Documents/org/ \
 && /opt/emacs/bin/emacs --batch \
      --eval "(require 'org)" \
      --eval "(defvar my/local-dir  \"/emacs/.config/emacs/.local\") \"Local state directory\")" \
      --eval "(setq org-confirm-babel-evaluate nil)" \
      --eval "(org-babel-tangle-file \"/emacs/.config/readme.org\")" || true

# ---- Install packages at build time ----
RUN cd /emacs/.config/emacs && HOME=/emacs /opt/emacs/bin/emacs --batch \
      --load build-packages.el

# ---- Build Mu from source ----
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgmime-3.0-dev libxapian-dev meson ninja-build \
 && rm -rf /var/lib/apt/lists/* \
 && curl -fsSL https://github.com/djcb/mu/archive/refs/tags/v1.12.9.tar.gz -o /tmp/mu.tar.gz \
 && tar -xzf /tmp/mu.tar.gz -C /tmp \
 && cd /tmp/mu-1.12.9 \
 && meson setup build --prefix=/opt/mu -Demacs=/opt/emacs/bin/emacs \
 && ninja -C build \
 && ninja -C build install \
 && echo "=== MU4E FILES FOUND ===" \
 && find /opt/mu -name "*.el" -o -name "mu4e*" | head -20 \
 && echo "=== MU DIRECTORY STRUCTURE ===" \
 && find /opt/mu -type d | head -10 \
 && echo "=== MU SHARE CONTENTS ===" \
 && ls -la /opt/mu/share/ || echo "No share directory" \
 && rm -rf /tmp/mu*

# =========================
# Stage 2: runtime (tiny)
# =========================
FROM ubuntu:22.04 AS runtime

ENV DEBIAN_FRONTEND=noninteractive TZ=UTC

# Runtime libs for Emacs (GTK/WebKitGTK, native-comp JIT runtime, image libs, TLS)
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libgtk-3-0 libwebkit2gtk-4.0-37 libharfbuzz0b \
    libjansson4 libgnutls30 libxml2 libsqlite3-0 \
    libgccjit0 libtinfo6 libtree-sitter0 \
    libjpeg8 libgif7 libpng16-16 libxpm4 librsvg2-2 \
    # Essential tools
    git zip unzip curl wget jq fzf pass zoxide \
    # Build tools for vterm and other native modules
    build-essential make gcc \
    # Mail, search, archive, PDF/diagram toolchain, TeX, spell
    maildir-utils msmtp ripgrep silversearcher-ag qpdf \
    # Mu runtime dependencies
    libgmime-3.0-0 libxapian30 \
    ispell wamerican \
    texlive texlive-latex-extra texlive-fonts-recommended texlive-xetex auctex \
    # pdf-tools server (epdfinfo)
    elpa-pdf-tools-server \
    # Java (for PlantUML & ditaa) + Node (for Mermaid CLI)
    openjdk-17-jre-headless \
    nodejs npm \
    graphviz \
    # Font utilities
    fontconfig \
    # vterm dependencies
    cmake libtool-bin libvterm-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

# Install JetBrains Mono Nerd Font
RUN mkdir -p /usr/share/fonts/truetype/jetbrains-mono-nerd \
 && curl -fsSL https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/JetBrainsMono.zip -o /tmp/JetBrainsMono.zip \
 && unzip /tmp/JetBrainsMono.zip -d /usr/share/fonts/truetype/jetbrains-mono-nerd/ \
 && fc-cache -fv \
 && rm /tmp/JetBrainsMono.zip


# Cleanly replace Ubuntu's Node 12 + npm with NodeSource Node 20 (includes npm)
RUN apt-get update && apt-get install -y --no-install-recommends curl ca-certificates gnupg \
 && apt-get remove -y --purge npm libnode-dev nodejs \
 && apt-get autoremove -y --purge \
 && rm -rf /var/lib/apt/lists/* \
 && curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
 && apt-get update && apt-get install -y --no-install-recommends nodejs \
 && node -v && npm -v \
 && rm -rf /var/lib/apt/lists/*

# Install modern Node.js (20.x LTS) from NodeSource
RUN apt-get update && apt-get install -y --no-install-recommends curl ca-certificates \
 && curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
 && apt-get install -y --no-install-recommends nodejs \
 && npm --version && node --version \
 && rm -rf /var/lib/apt/lists/*

# PlantUML & ditaa (use distro packages for stability/size) but don't delete lists
RUN apt-get update && apt-get install -y --no-install-recommends plantuml ditaa


# Mermaid CLI (will pull Puppeteer; increases size, but required)
RUN npm install -g @mermaid-js/mermaid-cli --no-audit --no-fund

# Copy Emacs + Mu + fully realized ~/.config/emacs from builder
COPY --from=builder --chmod=0755 /opt/emacs /opt/emacs
COPY --from=builder --chmod=0755 /opt/mu /opt/mu
COPY --from=builder --chmod=0755 /emacs/.config/emacs /emacs/.config/emacs

# QoL symlinks + workspace
RUN ln -s /opt/emacs/bin/emacs /usr/local/bin/emacs \
 && ln -s /opt/mu/bin/mu /usr/local/bin/mu

ENTRYPOINT ["/emacs/.config/emacs/entrypoint.sh"]

LABEL maintainer="Ioannis Canellos" \
      description="Ubuntu container for reproducible Emacs" \
      emacs.version="${EMACS_VERSION}" \
      mu.version="${MU_VERSION}"
