yaml-mode yaml-ts-mode

;; GitHub Actions
(gha-wf "name: " p n "on:" n> "push:" n> "branches: [" p "]" n> "pull_request:" n> "branches: [" p "]" n n "jobs:" n> q)
(gha-job p ":" n> "runs-on: ubuntu-latest" n> "steps:" n> q)
(gha-step "- name: " p n> "uses: " q)
(gha-run "- name: " p n> "run: |" n> q)

;; Docker Compose
(svc p ":" n> "image: " p n> "ports:" n> "- \"" p ":" q "\"")
(vol "volumes:" n> "- " p ":" q)
(env "environment:" n> "- " p "=" q)

;; Common patterns
(list "- " q)
(kv p ": " q)
(anchor "&" p n q)
(alias "<<: *" q)
