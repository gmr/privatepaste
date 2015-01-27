define(['backbone', 'moment'], function(Backbone, moment) {
  return Backbone.Model.extend({
    urlRoot: '/paste',
    defaults: {
      'hostname': location.hostname,
      'id': null,
      'owner': null,
      'created_at': moment().format(),
      'updated_at': null,
      'revision': 0,
      'ttl': 432000,
      'password': null,
      'views': 0,
      'syntax': 'none',
      'line_numbers': true,
      'content': ''
    },

    /**
    * Return a Moment object that specifies when the paste will expire
    *
    * @returns object
    */
    expirationMoment: function() {
      var startTime = this.get('last_updated_at') !== null ? this.get('last_updated_at') : this.get('created_at');
      return moment.unix(startTime + parseInt(this.get('ttl')));
    },

    /**
    * Return the URL for the paste based upon the current location
    *
    * @returns {string}
    */
    fullURL: function() {
      return document.location.protocol + '//' + document.location.hostname + '/' + this.get('id');
    }
  });
});
