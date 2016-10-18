mushu - an ssh connection manager for emacs
==============================================

`mushu` allows you to use emacs to launch ssh connections to remote hosts. It takes hosts from your ssh config file (located by default at `~/.ssh/config`), and creates an ssh client connection to the host in an emacs shell. Since it uses normal system `ssh` in the background, all the configuration from your ssh configuration file carries over.

# Features
* connect to hosts using partial and fuzzy matching using helm;
* move to another session - select using helm filtering;
* no need for separate logging, everything is recorded in an emacs text buffer;
* organization can be expanded using other emacs packages;

# Still to do:

* Integrate file editing with tramp;