define(['backbone',
        'underscore',
        'app/models/paste',
        'codemirror/lib/codemirror',
        'codemirror/addon/selection/active-line'],
function(Backbone, _, Paste, CodeMirror) {

  return Backbone.View.extend({

    events: {
      'change input[name="line_numbers"]': 'onLineNumToggle',
      'change input[name="secure-paste"]': 'onSecurePasteChange',
      'change #ttl':                       'onTTLChange',
      'change #syntax':                    'onSyntaxChange',
      'click #save':                       'savePaste'
    },

    syntax: {
      "c": {
        "name": "clike",
        "mime": "text/x-csrc",
        "keywords": {
          "useCPP": true
        }
      },
      "c++": {
        "name": "clike",
        "mime": "text/x-c++src",
        "keywords": {
          "useCPP": true
        }
      },
      "c#": {
        "name": "clike",
        "mime": "text/x-csharp",
        "keywords": {
          "useCPP": true
        }
      },
      "json": {
        "name": "javascript",
        "mime": "application/javascript",
        "keywords": {}
      },
      "java": {
        "name": "clike",
        "mime": "text/x-java",
        "keywords": {
          "useCPP": true
        }
      },
      "scala": {
        "name": "clike",
        "mime": "text/x-scala",
        "keywords": {
          "useCPP": true
        }
      }
    },

    initialize: function(){
      this.saveButton = this.$el.find('#save');
      this.redirect = this.$el.find('#redirect');
      this.model = new Paste();
      this.model.on('change', this.render, this);
      this.cm = CodeMirror(document.getElementById('editor'),
                           {autofocus: true,
                            lineWrapping: true,
                            lineNumbers: true,
                            matchBrackets: true,
                            styleActiveLine: true});
      this.cm.setSize('auto', '100%');
      this.cm.on('change', _.bind(function(editor, changes){
          if (!editor.isClean()) {
              this.model.set('content', editor.getValue());
          }
      }, this));
      this.render();
    },

    render: function() {
      var syntax = this.model.get('syntax');
      var syntaxName = this.syntax[syntax] !== undefined ? this.syntax[syntax].name : syntax;
      if (this.cm.getOption('mode') != syntaxName) this.changeSyntax(syntax, syntaxName);
      if (this.model.get('content').length > 10) {
          this.saveButton.removeClass('disabled');
      } else {
          this.saveButton.addClass('disabled');
      }
      return this;
    },

    savePaste: function() {
        console.log('Saving paste');
        this.model.save(null, {
            error: function(model, response, options) {
                console.log("Error saving model", response);
            },
            success: function(model, response, options){
                window.location.pathname = '/' + model.get('id');
            }
        });
    },

    changeSyntax: function(syntax, syntaxName) {
      if (syntax === 'none') {
        this.cm.setOption('mode', null);
      } else if (syntax !== syntaxName) {
        require(['codemirror/mode/' + syntaxName + '/' + syntaxName],
                _.bind(function(_mode) {
                    this.cm.setOption('mode', this.syntax[syntax]);
                }, this));
      } else if (syntax != 'none') {
        require(['codemirror/mode/' + syntax + '/' + syntax],
                _.bind(function(_mode) {
                    this.cm.setOption('mode', syntax);
                }, this));
      }
    },

    getSyntaxName: function(syntax) {
      if (this.syntax[syntax] !== undefined)
        return this.syntax[syntax].name
      return syntax;
    },

    onLineNumToggle: function(event) {
      this.model.set('line_numbers', event.target.value == 'on' ? true : false);
    },

    onSyntaxChange: function(event) {
      this.model.set('syntax', event.target.value);
    },

    onTTLChange: function(event) {
      this.model.set('ttl', event.target.value);
    }

  });

});
