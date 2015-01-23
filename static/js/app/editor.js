define(['backbone',
        'codemirror/lib/codemirror',
        'codemirror/addon/selection/active-line',
        'codemirror/addon/fold/foldcode',
        'codemirror/addon/fold/foldgutter',
        'codemirror/addon/fold/brace-fold',
        'codemirror/addon/fold/comment-fold',
        'codemirror/addon/fold/indent-fold',
        'codemirror/addon/fold/markdown-fold',
        'codemirror/addon/fold/xml-fold',
        'codemirror/addon/mode/loadmode'
        ],
function(Backbone, CodeMirror, ActiveLine, FC, FG, BF, CF, IF, MF, XF, LoadMode){

  return Backbone.View.extend({

    render: function() {
      console.log('Rendering!', this.el)
      this._cm = CodeMirror(this.el,
                            {autofocus: true,
                             lineWrapping: true,
                             lineNumbers: true,
                             matchBrackets: true,
                             modeURL: 'codemirror/mode/%N/%N.js',
                             styleActiveLine: true});
      this._cm.setSize('auto', '100%');
      this._cm.refresh();
    }
  });

});
