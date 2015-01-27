define(['backbone',
        'codemirror/lib/codemirror',
        'codemirror/addon/selection/active-line'],
function(Backbone, CodeMirror) {

  return Backbone.View.extend({

      initialize: function(){
          var lineNumbers = document.getElementById('editor').getAttribute('data-line-numbers') === 'true';
          var syntax = document.getElementById('paste-syntax').innerText;
          var self = this;
          require(['codemirror/mode/' + syntax + '/' + syntax], function(_mode){
            self.cm.setOption('mode', syntax);
          });
          this.cm = CodeMirror.fromTextArea(document.getElementById('paste-content'),
                                            {autofocus: true,
                                             lineNumbers: lineNumbers,
                                             readOnly: true,
                                             styleActiveLine: true});
          this.cm.setSize('auto', '100%');
      }
  });
});
