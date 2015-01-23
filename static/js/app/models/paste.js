define(['backbone'], function(Backbone) {
  return Backbone.Model.extend({
    urlRoot: '/paste',
    defaults: {
      "id": null,
      "syntax": 'none',
      "ttl": 432000,
      "password": null,
      "line_numbers": true,
      "code_folding": true,
      "views": 0,
      "revision": 0,
      "created_at": parseInt(new Date().getTime()/1000),
      "last_updated_at": null,
      "content": ''
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
