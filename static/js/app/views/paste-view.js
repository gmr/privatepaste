define(['backbone',
        'codemirror/lib/codemirror',
        'codemirror/addon/selection/active-line'],
function(Backbone, CodeMirror) {

  return Backbone.View.extend({

      initialize: function(){
          var self = this;
          var lineNumbers = document.getElementById('editor').getAttribute('data-line-numbers') === 'true';
          var syntax = document.getElementById('paste-syntax').getAttribute('data-value');
          this.cm = CodeMirror.fromTextArea(document.getElementById('paste-content'),
                                            {autofocus: true,
                                             lineNumbers: lineNumbers,
                                             readOnly: true,
                                             styleActiveLine: true});
          this.cm.setSize('auto', '100%');
          if (syntax != 'mode') {
              require(['codemirror/mode/' + syntax + '/' + syntax],
                      _.bind(function(_mode) {
                        this.cm.setOption('mode', syntax);
                    }, this));
          }
      }
  });
});
