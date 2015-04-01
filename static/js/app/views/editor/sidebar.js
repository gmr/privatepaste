define(['backbone',
        'lodash',
        'app/syntax'],
       function(Backbone, _, syntax) {
           return Backbone.View.extend({
               events: {
                    'change input[name="line_numbers"]': 'onLineNumToggle',
                    'change input[name="secure-paste"]': 'onSecurePasteChange',
                    'change #ttl':                       'onTTLChange',
                    'change #syntax':                    'onSyntaxChange',
                    'click #save':                       'savePaste',
                    'click label.active input[name="secure-paste"][value="on"]': 'onShowModal'
               },

               els: {},

               initialize: function() {
                   this.els.line_num_btn_off = this.$el.find('input[name="line_numbers"][value="off"]');
                   this.els.line_num_btn_on = this.$el.find('input[name="line_numbers"][value="on"]');
                   this.els.secure_btn_off = this.$el.find('input[name="secure-paste"][value="off"]');
                   this.els.secure_btn_on = this.$el.find('input[name="secure-paste"][value="on"]');

                   this.els.saveButton = this.$el.find('#save');
                   this.els.syntax = this.$el.find('#syntax');

                   this.model.on('change:content', this.toggleSaveButton, this);
                   this.model.on('change:line_numbers', this.render, this);
                   this.model.on('change:password', this.render, this);
                   this.model.on('change:syntax', this.render, this);
                   this.model.on('change:ttl', this.render, this);

                   this.render();
               },

               getSyntaxName: function(syntax) {
                   if (syntax[syntax] !== undefined)
                       return syntax[syntax].name;
                   return syntax;
               },

               onLineNumToggle: function(event) {
                   this.model.set('line_numbers', event.target.value == 'on');
               },

               onShowModal: function(event) {
                   console.log('onshowmodal');
                   Backbone.trigger('editor:secure-modal:show', true);
               },

               onSecurePasteChange: function(event) {
                   if (event.target.value == 'on') {
                       Backbone.trigger('editor:secure-modal:show', true);
                   } else {
                       this.model.set('password', null);
                       this.render();
                   }
               },

               onSyntaxChange: function(event) {
                   this.model.set('syntax', event.target.value);
               },

               onTTLChange: function(event) {
                   this.model.set('ttl', event.target.value);
               },

               render: function() {
                   this.toggleSaveButton();

                   if (this.model.hasChanged('line_numbers') === true) {
                       if (this.model.get('line_numbers') === false) {
                           this.els.line_num_btn_off.prop('checked', true).parents('label').addClass('active');
                           this.els.line_num_btn_on.parents('label').removeClass('active');
                       } else {
                           this.els.line_num_btn_on.prop('checked', true).parents('label').addClass('active');
                           this.els.line_num_btn_off.parents('label').removeClass('active');
                       }
                   }

                   if (this.model.get('password') == null && this.els.secure_btn_on.is(':checked') === true) {
                       this.els.secure_btn_off.prop('checked', true).parents('label').addClass('active');
                       this.els.secure_btn_on.parents('label').removeClass('active');
                   }

                   if (this.model.get('password') !== null && this.els.secure_btn_off.is(':checked') === true) {
                       this.els.secure_btn_on.prop('checked', true).parents('label').addClass('active');
                       this.els.secure_btn_off.parents('label').removeClass('active');
                   }

                   if (this.model.hasChanged('syntax') === true) {
                       var syntax = this.model.get('syntax');
                       var syntaxName = syntax[syntax] !== undefined ? syntax[syntax].name : syntax;
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

               toggleSaveButton: function() {
                   if (this.model.get('content').length > 10) {
                       this.els.saveButton.removeClass('disabled');
                   } else {
                       this.els.saveButton.addClass('disabled');
                   }
               }


           });
       });
