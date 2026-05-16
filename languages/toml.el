;;; toml.el --- TOML language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package toml
  :ensure nil
  :mode
  ("\\.toml\\'" . toml-ts-mode))
