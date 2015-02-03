define(['backbone',
        'underscore',
        'app/models/paste',
        'app/syntax',
        'codemirror/lib/codemirror',
        'codemirror/addon/selection/active-line'],
    function(Backbone, _, Paste, syntax, CodeMirror) {
        return Backbone.View.extend({

            events: {
                'change input[name="line_numbers"]':  'onLineNumToggle',
                'change input[name="secure-paste"]':  'onSecurePasteChange',
                'change #ttl':                        'onTTLChange',
                'change #syntax':                     'onSyntaxChange',
                'click #save':                        'savePaste'
            },

            initialize: function(){
                this.saveButton = this.$el.find('#save');
                this.redirect = this.$el.find('#redirect');
                this.model = new Paste({syntax: document.getElementById('syntax').value,
                                        ttl: document.getElementById('ttl').value});

                this.model.on('change', this.render, this);
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
                var syntax = this.model.get('syntax');
                var syntaxName = syntax[syntax] !== undefined ? syntax[syntax].name : syntax;
                if (this.cm.getOption('mode') != syntaxName) this.changeSyntax(syntax, syntaxName);
                if (this.model.get('content').length > 10) {
                    this.saveButton.removeClass('disabled');
                } else {
                    this.saveButton.addClass('disabled');
                }
                return this;
            },

            savePaste: function() {
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
                        this.cm.setOption('mode', syntax[syntax]);
                    }, this));
                } else if (syntax != 'none') {
                    require(['codemirror/mode/' + syntax + '/' + syntax],
                        _.bind(function(_mode) {
                        this.cm.setOption('mode', syntax);
                    }, this));
                }
            },

            getSyntaxName: function(syntax) {
                if (syntax[syntax] !== undefined)
                    return syntax[syntax].name;
                return syntax;
            },

            onLineNumToggle: function(event) {
                this.model.set('line_numbers', event.target.value == 'on');
            },

            onSyntaxChange: function(event) {
                this.model.set('syntax', event.target.value);
            },

            onTTLChange: function(event) {
                this.model.set('ttl', event.target.value);
            }
        });

});
