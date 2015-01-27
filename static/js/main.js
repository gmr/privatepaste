requirejs.config({
  baseUrl: '/static/js',
  paths: {
    backbone: 'vendor/backbone',
    bootstrap: 'vendor/bootstrap.min',
    jquery: 'vendor/jquery.min',
    underscore: 'vendor/underscore-min',
    moment: 'vendor/moment.min',
    codemirror: 'vendor/codemirror',

    editor: 'app/views/editor',
    pasteView: 'app/views/paste-view'
  },
  shim: {
    backbone: {
      deps: ['underscore', 'jquery'],
      exports: 'Backbone'
    },
    bootstrap: {
      deps: ['jquery'],
      exports: 'Bootstrap'
    },
    underscore: {
      exports: '_'
    }
  }
});
require(['jquery', 'underscore', 'backbone',  'bootstrap', 'editor', 'pasteView'],
  function($, _, Backbone, Bootstrap, Editor, PasteView) {
    var bind = document.body.getAttribute('data-bind');
    if (bind === "view") {
      var view = new PasteView({el: document.getElementById('content')});
    } else if (bind === "edit") {
      var editor = new Editor({el: document.getElementById('content')});
    }


});
