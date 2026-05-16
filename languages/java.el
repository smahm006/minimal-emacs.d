;;; java.el --- Java language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package java-ts-mode
  :ensure nil
  :mode ("\\.java\\'" . java-ts-mode)
  :hook
  (java-ts-mode . eglot-ensure)
  (java-ts-mode . (lambda ()
                    (define-key me/run-map (kbd "r") #'me/java-run)
                    (define-key me/run-map (kbd "t") #'me/java-test)
                    (define-key me/run-map (kbd "b") #'me/java-build)
                    (define-key me/run-map (kbd "C") #'me/java-clean)
                    (define-key me/run-map (kbd "f") #'me/java-format)))
  :preface
  (defun me/java-build-tool ()
    "Return the build tool symbol for the current project: `mvn', `gradle' or nil."
    (cond
     ((locate-dominating-file default-directory "pom.xml")          'mvn)
     ((locate-dominating-file default-directory "build.gradle")     'gradle)
     ((locate-dominating-file default-directory "build.gradle.kts") 'gradle)
     (t nil)))

  (defun me/java-project-root ()
    "Return the project root directory for the current Java project."
    (pcase (me/java-build-tool)
      ('mvn    (locate-dominating-file default-directory "pom.xml"))
      ('gradle (or (locate-dominating-file default-directory "build.gradle")
                   (locate-dominating-file default-directory "build.gradle.kts")))
      (_       default-directory)))

  (defun me/java-run ()
    "Run the current Java project using Maven or Gradle."
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
    "Run tests for the current Java project using Maven or Gradle."
    (interactive)
    (let ((default-directory (me/java-project-root)))
      (pcase (me/java-build-tool)
        ('mvn    (compile "mvn test"))
        ('gradle (compile "./gradlew test"))
        (_       (message "No build tool detected. Please use Maven or Gradle.")))))

  (defun me/java-build ()
    "Build the current Java project using Maven or Gradle."
    (interactive)
    (let ((default-directory (me/java-project-root)))
      (pcase (me/java-build-tool)
        ('mvn    (compile "mvn compile"))
        ('gradle (compile "./gradlew build"))
        (_       (compile (format "javac %s"
                                  (shell-quote-argument buffer-file-name)))))))

  (defun me/java-clean ()
    "Clean build artifacts using Maven or Gradle."
    (interactive)
    (let ((default-directory (me/java-project-root)))
      (pcase (me/java-build-tool)
        ('mvn    (compile "mvn clean"))
        ('gradle (compile "./gradlew clean"))
        (_       (message "No build tool detected.")))))

  (defun me/java-format ()
    "Format the current buffer using apheleia (google-java-format)."
    (interactive)
    (apheleia-format-buffer '(google-java-format)))

  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(java-ts-mode . ("jdtls")))))

;;; gradle-mode — syntax and commands for Gradle build files
(use-package gradle-mode
  :mode
  ("\\.gradle\\'"     . gradle-mode)
  ("\\.gradle.kts\\'" . gradle-mode))

;;; maven-test-mode — run Maven tests and navigate to failures
(use-package maven-test-mode
  :after java-ts-mode
  :hook
  (java-ts-mode . maven-test-mode)
  (java-ts-mode . (lambda ()
                    (define-key me/run-map (kbd "t a") #'maven-test-all)
                    (define-key me/run-map (kbd "t c") #'maven-test-class)
                    (define-key me/run-map (kbd "t f") #'maven-test-method)
                    (define-key me/run-map (kbd "t r") #'maven-test-rerun))))
