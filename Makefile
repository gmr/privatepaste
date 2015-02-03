LESSC = `which lessc`
REBAR = `which rebar`
RELX = `which relx`
XGETTEXT = `which xgettext`

NAME = PrivatePaste
COMPANY = Poison Pen LLC
VERSION = 2.0.0
YEAR = `date +%Y`
FULLNAME = `git config --global --get user.name`
EMAIL = `git config --global --get user.email`

CHARSET = ISO-8859-1
GETTEXT_DIR = translations
DEFAULT_LANGUAGE = en
PO_PATH = $(GETTEXT_DIR)/lang/default/$(DEFAULT_LANGUAGE)

BOOTSTRAP = bower_components/bootstrap
CODEMIRROR = bower_components/codemirror
FONTAWESOME = bower_components/font-awesome

LESS_INCLUDE = ${BOOTSTRAP}/less:${CODEMIRROR}/lib:${FONTAWESOME}/less:${CODEMIRROR}
LESS_IN = static/less/bootstrap.less
CSS_OUT = static/css/privatepaste.css


all: deps compile static po

bower:
	@( bower -s install )
	@( mkdir -p static/js/vendor )
	@( cp bower_components/backbone/backbone.js static/js/vendor/ )
	@( cp bower_components/bootstrap/dist/js/bootstrap.min.js static/js/vendor/ )
	@( cp bower_components/jquery/dist/jquery.min.js static/js/vendor/ )
	@( cp bower_components/requirejs/require.js static/js/ )
	@( cp bower_components/underscore/underscore-min.js static/js/vendor/ )
	@( cp bower_components/moment/min/moment.min.js static/js/vendor/ )
	@( cp -r $(CODEMIRROR) static/js/vendor/ )
	@( mkdir -p static/fonts )
	@( cp $(FONTAWESOME)/fonts/* static/fonts/ )

less:
	@( $(LESSC) --verbose --strict-imports --source-map=${CSS_OUT}.map --include-path=${LESS_INCLUDE} ${LESS_IN} ${CSS_OUT} )

static: less

deps: bower
	@( $(REBAR) get-deps )

compile:
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )
	@( rm -f $(CSS_OUT) )
	@( rm -f static/fonts/* )
	@( rm -f erl_crash.dump )
	@( rm -f $(PO_PATH)/gettext.po )
	@( rm -f translations/gettext_server_db.dets )

run:
	@( erl +W w -pa ebin deps/*/ebin -config rel/sys.config -sname privatepaste -sync log all -s privatepaste )

release: compile
	@( $(RELX) release )

po:
	@( mkdir -p $(PO_PATH) )
	@( $(XGETTEXT) -L c -k_ -d gettext -s -p $(PO_PATH) --package-name="$(NAME)" --package-version="$(VERSION)" templates/*.dtl templates/*/*.dtl templates/*/*/*.dtl )
	@( sed -i "" "s/YEAR/$(YEAR)/g" $(PO_PATH)/gettext.po )
	@( sed -i "" "s/THE PACKAGE'S COPYRIGHT HOLDER/$(COMPANY)/g" $(PO_PATH)/gettext.po )
	@( sed -i "" "s/SOME DESCRIPTIVE TITLE./$(NAME)/g" $(PO_PATH)/gettext.po )
	@( sed -i "" "s/PACKAGE/$(NAME)/g" $(PO_PATH)/gettext.po )
	@( sed -i "" "s/FIRST AUTHOR/$(FULLNAME)/g" $(PO_PATH)/gettext.po )
	@( sed -i "" "s/FULL NAME/$(FULLNAME)/g" $(PO_PATH)/gettext.po )
	@( sed -i "" "s/EMAIL@ADDRESS/$(EMAIL)/g" $(PO_PATH)/gettext.po )
	@( sed -i "" "s/CHARSET/$(CHARSET)/g" $(PO_PATH)/gettext.po )
	@( msgen -o $(PO_PATH)/gettext.po $(PO_PATH)/gettext.po )
	@( echo "Updated $(PO_PATH)/gettext.po" )

.PHONY: all deps compile clean run
