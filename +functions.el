  (defun iocanel/check-cves ()
    "Update the list of CVE issues"
    (interactive)
    (shell-command "/home/iocanel/scripts/work/collect-cve.sh ~/Documents/work/cve.org")
    (find-file "~/Documents/work/cve.org"))
