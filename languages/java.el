;;; java.el --- Java language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package java-ts-mode
  :ensure nil
  :mode ("\\.java\\'" . java-ts-mode)
  :bind
  (:map me/java-run-map
        ("r" . me/java-run) ("t" . me/java-test)
        ("b" . me/java-build) ("C" . me/java-clean))
  :preface
  (defun me/java-build-tool ()
    "Return the project build tool, or nil."
    (cond
     ((locate-dominating-file default-directory "pom.xml")          'mvn)
     ((locate-dominating-file default-directory "build.gradle")     'gradle)
     ((locate-dominating-file default-directory "build.gradle.kts") 'gradle)
     (t nil)))

  (defun me/java-project-root ()
    "Return the Java project root."
    (pcase (me/java-build-tool)
      ('mvn    (locate-dominating-file default-directory "pom.xml"))
      ('gradle (or (locate-dominating-file default-directory "build.gradle")
                   (locate-dominating-file default-directory "build.gradle.kts")))
      (_       default-directory)))

  (defun me/java-run ()
    "Run the Java project."
    (interactive)
    (let ((default-directory (me/java-project-root)))
      (pcase (me/java-build-tool)
        ('mvn    (compile "mvn compile exec:java"))
        ('gradle (compile "./gradlew run"))
        (_       (compile (format "javac %s && java %s"
                                  (shell-quote-argument buffer-file-name)
                                  (file-name-sans-extension
                                   (file-name-nondirectory buffer-file-name))))))))

  (defun me/java-test ()
    "Run Java tests."
    (interactive)
    (let ((default-directory (me/java-project-root)))
      (pcase (me/java-build-tool)
        ('mvn    (compile "mvn test"))
        ('gradle (compile "./gradlew test"))
        (_       (message "No build tool detected. Please use Maven or Gradle.")))))

  (defun me/java-build ()
    "Build the Java project."
    (interactive)
    (let ((default-directory (me/java-project-root)))
      (pcase (me/java-build-tool)
        ('mvn    (compile "mvn compile"))
        ('gradle (compile "./gradlew build"))
        (_       (compile (format "javac %s"
                                  (shell-quote-argument buffer-file-name)))))))

  (defun me/java-clean ()
    "Clean Java build files."
    (interactive)
    (let ((default-directory (me/java-project-root)))
      (pcase (me/java-build-tool)
        ('mvn    (compile "mvn clean"))
        ('gradle (compile "./gradlew clean"))
        (_       (message "No build tool detected.")))))

  :config
  (me/enable-run-map java-ts-mode-map me/java-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(java-ts-mode . ("jdtls")))))

;;; gradle-mode — syntax and commands for Gradle build files
(use-package gradle-mode
  :mode
  ("\\.gradle\\'"     . gradle-mode)
  ("\\.gradle.kts\\'" . gradle-mode))
