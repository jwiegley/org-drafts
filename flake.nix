{
  description = "org-drafts - Manage drafts using Org-capture";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      pkgsFor = system: nixpkgs.legacyPackages.${system};

      # Runtime dependencies needed by org-drafts.el
      runtimeDeps = epkgs: [
        epkgs.org
        epkgs.copy-as-format
        epkgs.pretty-hydra
      ];

      # Optional runtime dependencies
      optionalDeps = epkgs: [
        epkgs.gptel
      ];

      # Development/CI dependencies
      devDeps = epkgs: [
        epkgs.package-lint
      ];

      # Emacs with all runtime deps for building and checks
      emacsWithDepsFor = pkgs:
        let
          epkgs = pkgs.emacsPackagesFor pkgs.emacs;
        in
        epkgs.emacsWithPackages (ep:
          runtimeDeps ep ++ optionalDeps ep ++ devDeps ep
        );

      src = nixpkgs.lib.cleanSource ./.;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          epkgs = pkgs.emacsPackagesFor pkgs.emacs;
        in
        {
          default = epkgs.trivialBuild {
            pname = "org-drafts";
            version = "1.0";
            inherit src;
            packageRequires = runtimeDeps epkgs;
            # trivialBuild natively supports this flag, which sets
            # byte-compile-error-on-warn to t in the default build phase.
            turnCompilationWarningToError = true;
          };
        });

      checks = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          emacsWithDeps = emacsWithDepsFor pkgs;
        in
        {
          compile = pkgs.runCommand "org-drafts-compile" {
            nativeBuildInputs = [ emacsWithDeps ];
          } ''
            cp -r ${src}/* .
            chmod -R u+w .
            emacs --batch -Q -L . \
              --eval '(setq byte-compile-error-on-warn t)' \
              -f batch-byte-compile org-drafts.el
            touch $out
          '';

          lint = pkgs.runCommand "org-drafts-lint" {
            nativeBuildInputs = [ emacsWithDeps ];
          } ''
            cp -r ${src}/* .
            chmod -R u+w .
            emacs --batch -Q -L . \
              --eval '(require (quote package-lint))' \
              --eval '(setq package-lint-main-file "org-drafts.el")' \
              -f package-lint-batch-and-exit \
              org-drafts.el
            touch $out
          '';

          checkdoc = pkgs.runCommand "org-drafts-checkdoc" {
            nativeBuildInputs = [ emacsWithDeps ];
          } ''
            cp -r ${src}/* .
            chmod -R u+w .
            emacs --batch -Q -L . \
              --eval '
                (progn
                  (with-current-buffer (find-file-noselect "org-drafts.el")
                    (let ((warnings (checkdoc-file "org-drafts.el")))
                      (when (stringp warnings) (kill-emacs 1)))))
              '
            touch $out
          '';

          format-check = pkgs.runCommand "org-drafts-format-check" {
            nativeBuildInputs = [ emacsWithDeps ];
          } ''
            cp -r ${src}/* .
            chmod -R u+w .
            emacs --batch -Q -L . \
              --eval '(setq-default indent-tabs-mode nil)' \
              --eval '(put (quote org-drafts-with-change-to) (quote lisp-indent-function) 1)' \
              --eval '(put (quote org-drafts-with) (quote lisp-indent-function) 2)' \
              --eval '(put (quote pretty-hydra-define) (quote lisp-indent-function) (quote defun))' \
              --eval '
                (with-temp-buffer
                  (insert-file-contents "org-drafts.el")
                  (let ((original (buffer-string)))
                    (emacs-lisp-mode)
                    (indent-region (point-min) (point-max))
                    (unless (string= original (buffer-string))
                      (message "Formatting differs")
                      (kill-emacs 1))))
              '
            touch $out
          '';

          test = pkgs.runCommand "org-drafts-test" {
            nativeBuildInputs = [ emacsWithDeps ];
          } ''
            cp -r ${src}/* .
            chmod -R u+w .
            emacs --batch -Q -L . \
              -l org-drafts-test.el \
              -f ert-run-tests-batch-and-exit
            touch $out
          '';
        });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          emacsWithDeps = emacsWithDepsFor pkgs;
        in
        {
          default = pkgs.mkShell {
            buildInputs = [
              emacsWithDeps
              pkgs.lefthook
            ];

            shellHook = ''
              echo "org-drafts dev shell"
              echo "  emacs:    $(emacs --version | head -1)"
              echo "  lefthook: $(lefthook version)"
            '';
          };
        });
    };
}
