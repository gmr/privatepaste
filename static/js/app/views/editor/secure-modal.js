define(['backbone'],
       function(Backbone) {
           return Backbone.View.extend({
               events: {
                   'click .btn-cancel': 'onCancel',
                   'click button[data-target="secure-password"]': 'onGeneratePassword',
                   'change input[data-bind="password"]': 'onPasswordChange',
                   'blur input[data-bind="password"]': 'onPasswordChange'
               },

               initialize: function() {
                   this.password = this.$el.find('input[data-bind="password"]');
                   this.model.on('change:password', this.render, this);
               },

               render: function() {
                   if (this.model.hasChanged('password') === true && this.model.get('password') != this.password.val()){
                       this.password.val(this.model.get('password'));
                   }
               },

               onCancel: function(event) {
                   // Hack to force the sidebar to re-render if the password was never changed from null
                   if (this.model.get('password') === null) this.model.set('password', 1);
                   this.model.set('password', null);
                   this.$el.modal('hide');
               },

               onPasswordChange: function(event) {
                   var value = event.target.value !== '' ? event.target.value : null;
                   if (this.model.hasChanged('password') !== true && this.model.get('password') != value) {
                       this.model.set('password', value);
                   }
               },

               onGeneratePassword: function(e) {
                   this.model.set('password', this.generatePassword());
                   this.password.focus().select();
               },

               show: function() {
                   this.$el.modal();
               },

               /**
                * Generate a random password
                * Borrowed/lifted from Gumbo's answer at
                * http://stackoverflow.com/questions/1497481/javascript-password-generator
                *
                * @returns {String}
                */
               generatePassword: function() {
                   var length = 8,
                   charset = "abcdefghijklnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
                   retVal = "";
                   for (var i = 0, n = charset.length; i < length; ++i) {
                       retVal += charset.charAt(Math.floor(Math.random() * n));
                   }
                   return retVal;
               }
           });
       });
