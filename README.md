PrivatePaste.com
================
Repository for v2 of PrivatePaste.com which will be developed in an open-source
model.

Build requirements
------------------
- Erlang
- gettext (including xgettext app)
- libmagic (if installed via brew on OS X, make sure that /usr/local is symlinked to /opt/local to compile emagic)
- nodejs && npm
  - bower
  - less

Multi-node development in vagrant requires the vagrant-hosts plugin.

TODO
----

### MVP
- ~~Add link to Github project in footer~~
- ~~Add Remote IP address to paste~~
- ~~Add HTTP request headers to paste~~
- ~~Add mime-type to paste~~
- ~~Update sanitize to omit request headers and remote ip~~
- Notification of new paste URL on save
- Copy Paste
- Disable Edit
- Content Scanning / Blocking
- Sidebar created at/updated at values as relative values
- Session / Secure Cookie
 - Config based secure cookie signing salt
 - Flagged clients in browser get recaptcha
 - Recaptcha
- ~~Secure Paste password entry~~
- Report Paste
- DB Import for current version
- Hostname specific settings
 - OSS Icons for header icon
 - Default syntax
 - Default ttl override
- Travis build to github release

### Post-Beta Launch
- Login
 - Local accounts
  - Email Address Validation
 - OAuth
- Bypass content flagging with validated account
- Takedown Paste with validated account
- DB Backup

### Libs to look into/use for things
- https://github.com/ferd/erlpass.git
- https://github.com/tim/erlang-oauth.git
