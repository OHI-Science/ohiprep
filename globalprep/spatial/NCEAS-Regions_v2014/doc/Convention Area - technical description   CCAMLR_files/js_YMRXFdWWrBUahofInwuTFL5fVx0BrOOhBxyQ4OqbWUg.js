jQuery(document).ready(function() {
	
	jQuery(".notification-no-image").append("<div class=\"notification-icon\"></div>");
	
	// Appending icons to notification messages
	jQuery("#messages .warning").append("<div class=\"messages-warning-icon\"></div>");
	jQuery("#messages .error").append("<div class=\"messages-error-icon\"></div>");
	jQuery("#messages .status").append("<div class=\"messages-status-icon\"></div>");
	
	
	// Default values & classes for search field
	/*jQuery('#search-block-form .form-text').each(function() {
		this.value = 'Search the site';
    	var form_text = this.value;
    	jQuery(this).addClass('default-value'); // this could be in the style sheet instead
    	jQuery(this).focus(function() {
        	if(this.value == 'Search the site') {
            	this.value = '';
            	jQuery(this).removeClass('default-value');
        	}
    	});
    
    
    jQuery(this).blur(function() {
        if(this.value == '') {
			jQuery(this).addClass('default-value');
            this.value = 'Search the site';
        }
    });
	});*/
	
	// The first line below uses the label as the default value whereas the 
	// second line uses custom text
	//jQuery('#main-content .form-item').innerLabel();
	//jQuery('.form-item').innerLabel('Search this site...');
	
	
	// Seems to affect firefox negatively
	// To render button down state in webkit browsers
	/*jQuery(".form-submit").click(
		function () { jQuery(this).addClass("focus"); 
		}
	);*/
	
	//jQuery("#messages .tabledrag-changed-warning").append("<div class=\"messages-warning-icon\"></div>"); can't load post ajax

});
;
jQuery(document).ready(function() {


//fix for sub-menus not expanding
//show parent menu as expanded when on the child menu item
jQuery("div.block-menu-block .active").parents("ul")
						.addClass("expanded")
						.addClass("dhtml-menu-open")
						.removeClass("collapsed")
						.show()
						.end()
						.parents("li.collapsed")
						.removeClass("colapsed")
						.addClass("expanded")
						.end()
						.closest("li")
						.addClass('active')
						.removeClass("expanded");

//expand the menu if it has child elements,//if there is a sub-menu
if (jQuery("div.block-menu-block li.active").children('ul.menu').length>0) {
  jQuery("div.block-menu-block li.active").removeClass('collapsed').addClass('expanded').children('ul.menu').show();
}

// BOOKMARK SITE
function bookmark_us(url, title){

if (window.sidebar) // firefox
    window.sidebar.addPanel(title, url, "");
else if(window.opera && window.print){ // opera
    var elem = document.createElement('a');
    elem.setAttribute('href',url);
    elem.setAttribute('title',title);
    elem.setAttribute('rel','sidebar');
    elem.click();
} 
else if(document.all)// ie
    window.external.AddFavorite(url, title);
}

});

// INCREASE FONT SIZE
var min=10;
var max=14;
function increaseFontSize(elemId) {
   var p = document.getElementById(elemId);
//   for(i=0;i<p.length;i++) {    // use a loop if using classnames instead of elementId
      if(p.style.fontSize) {
         var s = parseFloat(p.style.fontSize.replace("px",""));
      } else {
         var s = 11;
      }
      if(s!=max) {
         s += 1;
      }
      p.style.fontSize = s+"px"
//   }
}

// DECREASE FONT SIZE
function decreaseFontSize(elemId) {
   var p = document.getElementById(elemId);
   //for(i=0;i<p.length;i++) { // use a loop if using classnames instead of elementId 
      if(p.style.fontSize) {
         var s = parseFloat(p.style.fontSize.replace("px",""));
      } else {
         var s = 11;
      }
      if(s!=min) {
         s -= 1;
      }
      p.style.fontSize = s+"px"
  // }   
}

// Pick up false triggers in menu hover
jQuery(document).ready(function()
{
  jQuery('.dhtml-menu-icon').hover(
    function() {
      jQuery(this).parent().addClass("false-trigger");
    },
    function() {
      jQuery(this).parent().removeClass("false-trigger");
    }
  );
});

jQuery("#main-content table.views-table").after("<div class=\"table-shadow\"></div>");
jQuery("table.formatted-table tr:even").addClass("even");
jQuery("table.formatted-table tr:odd").addClass("odd");;
