define(['backbone',
        'app/models/paste',
        'codemirror/lib/codemirror',
        'codemirror/addon/selection/active-line',
        'codemirror/addon/fold/foldcode',
        'codemirror/addon/fold/foldgutter',
        'codemirror/addon/fold/brace-fold',
        'codemirror/addon/fold/comment-fold',
        'codemirror/addon/fold/indent-fold',
        'codemirror/addon/fold/markdown-fold',
        'codemirror/addon/fold/xml-fold',
        'codemirror/addon/mode/loadmode'],
function(Backbone, Paste, CodeMirror) {

  return Backbone.View.extend({

    events: {
      'change input[name="code_folding"]': 'onCodeFoldingToggle',
      'change input[name="line_numbers"]': 'onLineNumToggle',
      'change input[name="secure-paste"]': 'onSecurePasteChange',
      'change #ttl':                       'onTTLChange',
      'change #syntax':                    'onSyntaxChange'
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
      this.setGutters();
      if (this.model.get('content').length > 10) {
        console.log('enabled button');
        this.saveButton.removeClass('disabled');
      } else {
        console.log('disabled button');
        this.saveButton.addClass('disabled');
      }
      return this;
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

    foldCode: function() {
      var fold;
      var syntax = this.model.get('syntax');
      var indentFold = ['erlang', 'python', 'ruby', 'yaml']
      if (indentFold.indexOf(syntax) > -1) {
        fold = new CodeMirror.fold.combine(CodeMirror.fold.comment, CodeMirror.fold.indent);
      } else if (this.syntax == 'xml' || this.syntax == 'htmlmixed') {
        fold = new CodeMirror.fold.combine(CodeMirror.fold.comment, CodeMirror.fold.xml);
      } else {
        fold = new CodeMirror.fold.combine(CodeMirror.fold.comment, CodeMirror.fold.brace);
      }
      this.cm.setOption('foldGutter', {rangeFinder: fold, scanUp: true});

    },

    getSyntaxName: function(syntax) {
      if (this.syntax[syntax] !== undefined)
        return this.syntax[syntax].name
      return syntax;
    },

    onCodeFoldingToggle: function(event) {
      this.model.set('code_folding', event.target.value == 'on' ? true : false);
    },

    onLineNumToggle: function(event) {
      this.model.set('line_numbers', event.target.value == 'on' ? true : false);
    },

    onSyntaxChange: function(event) {
      this.model.set('syntax', event.target.value);
    },

    onTTLChange: function(event) {
      this.model.set('ttl', event.target.value);
    },

    setGutters: function(model) {
      var gutters = [];
      if (this.model.get('line_numbers') === true) {
        gutters.push('CodeMirror-linenumbers');
      }
      this.cm.setOption('lineNumbers', this.model.get('line_numbers'));
      if (this.model.get('code_folding') === true)
      {
        gutters.push('CodeMirror-foldgutter');
        this.foldCode();
      } else {
        this.unfoldCode();
      }
      this.cm.setOption('gutters', gutters);
    },

    unfoldCode: function() {

      // Turn off code folding in the gutter
      this.cm.setOption('foldGutter', false);

      // Get all the marks in the document and if they're a code folding mark, remove it
      var marks = this.cm.getDoc().getAllMarks();
      for (var offset = 0; offset < marks.length; offset++)
      {
        if (marks[offset].__isFold) marks[offset].clear();
      }

      // Remove the markers from the gutter
      this.cm.clearGutter('CodeMirror-foldgutter');
    }

  });

});
