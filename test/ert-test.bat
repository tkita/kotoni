@echo off
setlocal
set EMACS=C:/usr/emacs-30.2/bin/emacs-30.2.exe
"%EMACS%" --quick --batch -L .. --load ert --load ktn-test.el --funcall ert-run-tests-batch-and-exit

pause
