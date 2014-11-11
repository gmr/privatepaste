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

LESS_INCLUDE = ${BOOTSTRAP}/less:${CODEMIRROR}/lib:${FONTAWESOME}/less
LESS_IN = static/less/bootstrap.less
CSS_OUT = static/css/privatepaste.css


all: deps compile static po

less:
	@( $(LESSC) --verbose -x --source-map=${CSS_OUT}.map --include-path=${LESS_INCLUDE} ${LESS_IN} ${CSS_OUT} )

fonts:
	@( cp ${FONTAWESOME}/fonts/* static/fonts/ )

static: less fonts

deps:
	@( $(REBAR) get-deps )

compile: clean
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )
	@( rm -f static/css/* )
	@( rm -f static/fonts/* )
	@( rm -f erl_crash.dump )
	@( rm -f $(PO_PATH)/gettext.po )
	@( rm translations/gettext_server_db.dets )

run:
	@( erl -pa  ebin deps/*/ebin -s privatepaste )

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
