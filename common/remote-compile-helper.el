
(defun run-cmd (cmd)
  "Run a shell command and return its output.  This removes all
newlines; intended to be used for commands which emit only one
line of output."
  (replace-regexp-in-string
   "\n" ""
   (shell-command-to-string cmd))
  )

(defun find-git-root-from (local-path)
  "Given a local path, try to locate the root of a Git repository
clone by searching the directory hierarchy."
  (interactive)
  (if (equal local-path "/")
      nil
    (let ((curPath (concat local-path "/.git")))
      (if (file-directory-p curPath)
          (progn (message local-path) local-path)
        (find-git-root-from (expand-file-name (concat local-path "../")))
        )
      )
    )
  )

(defun find-git-root ()
  "Using the current buffer's file path as a starting point, try
to locate the root of a Git repository clone by searching the
directory hierarchy."
  (interactive)
  (find-git-root-from
   (file-name-directory
    (or load-file-name buffer-file-name)))
  )

(defun get-filesystem-remote-host (local-path)
  "Given a local path within a mounted SSHFS filesystem, return
the hostname to which the local path is mounted."
  (interactive)
  (run-cmd
   (concat "df " local-path " | grep -v Filesystem | cut -d: -f 1"))
  )

(defun get-filesystem-remote-path (local-path)
  "Given a local path within a mounted SSHFS filesystem, return
the remote path to which the local path is mounted."
  (interactive)
  (run-cmd
   (concat "df " local-path " | grep -v Filesystem | cut -d\\  -f 1 | cut -d: -f 2"))
  )

(defun get-filesystem-local-path (local-path)
  "Given a local path within a mounted SSHFS filesystem, return
the local mount point path."
  (interactive)
  (run-cmd
   (concat "df " local-path " | grep -v Filesystem | awk '{ print $6 }'"))
  )

(defun remote-path-from-local-path (local-path)
  "Given a local path within a mounted SSHFS filesystem, return
the full remote path corresponding to the local path."
  (interactive)
  (concat
   (get-filesystem-remote-path local-path)
   (substring local-path (length (get-filesystem-local-path local-path)))
   )
  )

(defun run-cabaldev-install ()
  "Run 'cabal-dev install' on a remote host.  This looks at the
current buffer's local disk file path to determine the Git
repository root, remote host, and remote path to use when running
'cabal-dev'."
  (interactive)
  (let ((repo-root (find-git-root)))
    (if repo-root
        (progn
          (message (concat "Repo root found, is: " repo-root))
          (setq remote-shell-program "/opt/local/bin/ssh")
          (message "About to run compile (defaut-dir = " repo-root ")")
          (let ((default-directory repo-root)
                (remote-host (get-filesystem-remote-host repo-root))
                (remote-path (remote-path-from-local-path repo-root))
                (local-user (getenv "USER"))
                )
            (remote-compile
             remote-host
             local-user
             (concat "cd " remote-path " && cabal-dev install"))
            )
          )
      (progn
        (message "This buffer's file is not located in a git repository clone.")
        nil
        )
      )
    )
  )

;; Run cabal-dev remotely in another window.
(defun cabaldev-compile ()
  "Like 'compile', except that this uses 'cabal-dev' and assumes
that the buffer in which this is invoked corresponds to a disk
file in a Git repository on an SSHFS filesystem.  The remote host
will be contacted with SSH and must have 'cabal-dev' installed."
  (interactive)
  (if (run-cabaldev-install)
      (progn
        (other-window 1)
        (end-of-buffer)
        (other-window 1)
        )
    )
  )
