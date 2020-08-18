;;; quickmarks.el --- Quickmarks



;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(defvar quickmarks-alist '() "A list of labeled links for quick access")
(setq quickmarks-alist '(
                         ;; personal links
                         (blog . https://github.com/iocanel/blog)
                         (presentations . https://github.com/iocanel/presentations)
                         ;; emacs
                         (markdown . https://en.wikipedia.org/wiki/Markdown)
                         (org-mode . https://orgmode.org)
                         (emacs . http://emacs.org)
                         (spacemacs . http://spacemacs.org)
                         (yasnippets . https://github.com/joaotavora/yasnippet)
                         (straight-use-package . https://github.com/raxod502/straight.el)
                         ;; hardware
                         (ThinkPad\ P50 . https://www.lenovo.com/eg/en/laptops/thinkpad/p-series/ThinkPad-P50/p/22TP2WPWP50)
                         (Ergodox\ EZ . https://ergodox-ez.com)
                         (Signum\ 3 . http://troyfletcher.net/keyboard_sales.html\#signum_30)
                         (My\ Ergodox\ Config . https://configure.ergodox-ez.com/ergodox-ez/layouts/rwMQ/latest/0)
                         ;; linux
                         (archlinux . https://archlinux.org)
                         (dotfiles . https://github.com/iocanel/dotfiles)
                         (i3 . https://i3wm.org)
                         (mutt . http://www.mutt.org)
                         (weechat . https://weechat.org)
                         (qutebrowser . https://qutebrowser.org)
                         (ranger . https://github.com/ranger/ranger)
                         ;; work
                         (ap4k . https://github.com/ap4k/ap4k)
                         (arquillian-cube . https://github.com/arquillian/arquillian-cube)
                         (dagger . https://github.com/square/dagger)
                         (dekorate . https://github.com/dekorateio/dekorate)
                         (docker . https://docker.io)
                         (Eclipse\ IDE .  https://www.eclipse.org/ide)
                         (elasticsearch . https://www.elastic.co)
                         (elasticsearch\ opertor . https://github.com/upmc-enterprises/elasticsearch-operator)
                         (fabric8 . https://fabric8.io)
                         (fabric8\ maven\ plugin . https://maven.fabric8.io)
                         (fabric8-maven-plugin . https://maven.fabric8.io)
                         (fluentd . https://www.fluentd.org)
                         (gradle . https://gradle.org)
                         (grails . https://grails.org)
                         (grafana . https://grafana.com)
                         (istio . https://istio.io)
                         (kafka . https://kafka.apache.org)
                         (kibana . https://www.elastic.co/products/kibana)
                         (kotlin . https://kotlinlang.org)
                         (kubernetes . https://kubernetes.io)
                         (metaparticle . https://metaparticle.io)
                         (maven . https://maven.apache.org)
                         (microsoft\ sql\ server . https://www.microsoft.com/en-us/sql-server/sql-server-2017)
                         (micronaut . http://micronaut.io)
                         (openshift . https://openshift.com)
                         (open\ service\ broker\ api . https://www.openservicebrokerapi.org)
                         (prometheus . https://prometheus.io)
                         (prometheus\ operator . https://github.com/coreos/prometheus-operator)
                         (service\ catalog . https://svc-cat.io)
                         (service\ catalog\ connector . https://github.com/snowdrop/servicecatalog-connector)
                         (sdkman . https://sdkman.io)
                         (snowdrop . https://snowdrop.me)
                         (spring . https://spring.io)
                         (spring\ cloud . https://cloud.spring.io)
                         (spring\ intializer . https://start.spring.io)
                         (spring\ boot . https://spring.io/projects/spring-boot)
                         (spring\ cloud\ connectors . https://cloud.spring.io/spring-cloud-connectors)
                         (spring\ cloud\ kubernetes . https://github.com/spring-cloud/spring-cloud-kubernetes)
                         (spring\ data . https://projects.spring.io/spring-data/)
                         (thorntail . https://thorntail.io)
                         (jenkins . https://jenkins.io)
                         (jaeger . https://www.jaegertracing.io)
                         (jaeger-agent . https://www.jaegertracing.io/docs/1.10/deployment/\#agent)
                         (jaeger\ operator . https://github.com/jaegertracing/jaeger-operator)
                         (quarkus . https://quarkus.io)
                         (RHOAR . https://developers.redhat.com/products/rhoar/overview)
                         ;; kubernetes/openshift resources
                         (DeploymentConfig . https://docs.openshift.com/enterprise/3.0/dev_guide/deployments.html)
                         (Route . https://docs.openshift.com/enterprise/3.0/dev_guide/routes.html)
                         ;; Emacs
                         (lsp-java . https://github.com/emacs-lsp/lsp-java)
                         (treemacs . https://github.com/Alexander-Miller/treemacs)
                        ;; People
                         (Brendand\ Burns . https://twitter.com/brendandburns)
                         (Georgios\ Andrianakis . https://twitter.com/geoand86)
                         (Dimitris\ Andreadis . https://twitter.com/dandreadis)
                         (Rolland\ Huss . https://ro14nd.de/about)
                         (Troy\ Fletcher . https://www.youtube.com/channel/UCjtyO3ymIvrkz4ls_ro22fw)
                         ;; Hobbies
                         (anki . https://apps.ankiweb.net)
                         ;; Conferences
                         (Voxxed\ Athens . https://voxxeddays.com/athens)
                         )
      )

(defun quickmarks-get (k)
  "Get the value of the quickmark with the key K."
  (alist-get (intern k) quickmarks-alist)
  )

(provide 'quickmarks)
;;; quickmarks.el ends here
