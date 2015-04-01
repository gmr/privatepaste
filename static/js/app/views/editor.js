define(['backbone',
        'lodash',
        'app/models/paste',
        'app/syntax',
        'app/views/editor/secure-modal',
        'app/views/editor/sidebar',
        'codemirror/lib/codemirror',
        'codemirror/addon/selection/active-line'],
    function(Backbone, _, Paste, syntax,  SecureModal, Sidebar, CodeMirror) {
        return Backbone.View.extend({

            initialize: function(){
                this.redirect = this.$el.find('#redirect');

                this.model = new Paste({syntax: document.getElementById('syntax').value,
                                        ttl: document.getElementById('ttl').value});

                this.sidebar = new Sidebar({el: document.getElementById('sidebar'),
                                            model: this.model});

                this.secureModal = new SecureModal({el: this.$el.find('#secure-modal'),
                                                    model: this.model});

                this.model.on('change:line_numbers', this.render, this);
                this.model.on('change:syntax', this.render, this);
                this.listenTo(Backbone, 'editor:secure-modal:show',
                              _.bind(function(e) {
                                         this.secureModal.show();
                                     }, this));

                this.cm = CodeMirror(document.getElementById('codemirror'),
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
                // Maybe change the syntax
                if (this.model.hasChanged('syntax') === true) {
                    var syntax = this.model.get('syntax');
                    var syntaxName = syntax[syntax] !== undefined ? syntax[syntax].name : syntax;
                    if (this.cm.getOption('mode') != syntaxName) this.changeSyntax(syntax, syntaxName);
                }

                // Maybe change the line number and gutter settings
                if (this.model.hasChanged('line_numbers') === true) {
                    var lineNumbers = this.model.get('line_numbers');
                    if (this.cm.getOption('lineNumbers') != lineNumbers) {
                        var gutters = [];
                        if (lineNumbers === true) gutters.push('CodeMirror-linenumbers');
                        this.cm.setOption('lineNumbers', lineNumbers);
                        this.cm.setOption('gutters', gutters);
                    }
                }

                return this;
            },

            changeSyntax: function(syntax, syntaxName) {
                if (syntax === 'none') {
                    this.cm.setOption('mode', null);
                } else if (syntax !== syntaxName) {
                    require(['codemirror/mode/' + syntaxName + '/' + syntaxName],
                    _.bind(function(_mode) {
                        this.cm.setOption('mode', syntax[syntax]);
                    }, this));
                } else if (syntax != 'none') {
                    require(['codemirror/mode/' + syntax + '/' + syntax],
                            _.bind(function(_mode) {
                                        this.cm.setOption('mode', syntax);
                                   }, this));
                }
            }
        });
});
