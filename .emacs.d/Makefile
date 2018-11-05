prepare:
	curl http://www.randomsample.de/profile-dotemacs.el --output profile-dotemacs.el

profile:
	emacs -Q -l ./profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
	  (setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs
