requirejs.config({
  baseUrl: '/static/js',
  paths: {
    backbone: 'vendor/backbone',
    bootstrap: 'vendor/bootstrap.min',
    jquery: 'vendor/jquery.min',
    underscore: 'vendor/underscore-min',
    codemirror: 'vendor/codemirror',
    editor: 'app/views/editor'


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
require(['jquery', 'underscore', 'backbone',  'bootstrap', 'editor'],
  function($, _, Backbone, Bootstrap, Editor) {
  var editor = new Editor({el: document.getElementById('content')});
});
