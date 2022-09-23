Emacs Configuration Generator
=============================

Find here the source for an Emacs configuration generator website.  It
is written in Common Lisp, and was tested using [SBCL].

A live instance ought to be available here: <https://emacs.amodernist.com/>.

[SBCL]:
    http://www.sbcl.org/

How to start
------------

Using [GNU Guix], you can start a server as follows:

    guix shell --pure -CN -m manifest.scm -- \
       sbcl --load ecg.lisp --eval "(start)"

The server HTTP will now be listening for new requests on port 9095.

[GNU Guix]:
    https://guix.gnu.org/

Contact
-------

Questions, issues, complaints, comments, insults, patches, etc. should
be sent to [public inbox].

[public inbox]:
    https://lists.sr.ht/~pkal/public-inbox

State of testing
----------------

The site has been tested (to differing degrees) with the following
browsers:

- GNU IceCat (91.7.0esr)
- Mozilla Firefox (91.7.0esr)
- eww (Emacs 29.0.50)
- netsurf (3.10)
- Google Chromium (99.0.4844.74)
- Fennec F-Droid (98.2.0)
