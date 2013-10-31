var ALL_ATTR_URL = "/attributes";
var getAttr;

$(document).ready(function() {    

    function compareNames(a,b) { 
	return a.name < b.name ? -1 : a.name > b.name ? 1 : 0;
    }

    getAttr = function (term, callback) {
	var result = [];
	$.getJSON(ALL_ATTR_URL
		  , {term: term}
		  , function(json) {
		      result = $.map(json.sort(compareNames)
				     , function(item) { return item.name; });
		      callback(result);
		  });
    }

    function add_autocomplete() {
	$("#attribute-seek").autocomplete({
	    source: function(req, res) {
		getAttr(req.term, res); 
	    },
	    messages: {
		noResults: '',
		results: function() {}
	    },
	    response: function(event, ui) {
		return ui.content.slice(0, 4);
	    },
	    select: function(event, ui) { 
		$("#attributes").append($("<span>").html(ui.item.label).addClass("selected-span")); 
		$("#log").text(ui.item.label + ": " + ui.item.value);
	    },
	});
    }

    add_autocomplete();

    
});


