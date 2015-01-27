define(['backbone',
        'app/models/paste',
        'codemirror/lib/codemirror',
        'codemirror/addon/selection/active-line'],
function(Backbone, Paste, CodeMirror) {

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
      this.model = new Paste();
      this.model.on('change', this.render, this);
      this.cm = CodeMirror(document.getElementById('editor'),
                           {autofocus: true,
                            lineWrapping: true,
                            lineNumbers: true,
                            matchBrackets: true,
                            styleActiveLine: true});
      this.cm.setSize('auto', '100%');
      var self = this;
      this.cm.on('change', function(editor, changes){
        if (!editor.isClean()) {
          self.model.set('content', editor.getValue());
        }
      });
      this.render();
    },

    render: function() {
      var syntax = this.model.get('syntax');
      var syntaxName = this.syntax[syntax] !== undefined ? this.syntax[syntax].name : syntax;
      if (this.cm.getOption('mode') != syntaxName) this.changeSyntax(syntax, syntaxName);
      if (this.model.get('content').length > 10) {
        console.log('enabled button');
        this.saveButton.removeClass('disabled');
      } else {
        console.log('disabled button');
        this.saveButton.addClass('disabled');
      }
      return this;
    },

    savePaste: function() {
        this.model.save();
    },

    changeSyntax: function(syntax, syntaxName) {
      var self = this;
      if (syntax === 'none') {
        this.cm.setOption('mode', null);
      } else if (syntax !== syntaxName) {
        require(['codemirror/mode/' + syntaxName + '/' + syntaxName], function(_mode){
          self.cm.setOption('mode', self.syntax[syntax]);
        });
      } else if (syntax != 'none') {
        require(['codemirror/mode/' + syntax + '/' + syntax], function(_mode){
          self.cm.setOption('mode', syntax);
        });
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
