
 	 //Cufon
	  
	    Cufon.replace("#categories  > ul > li > a");
  // Cufon.replace('.bluetext');
			Cufon.replace('.headerightext');
			Cufon.replace('.ha');
			Cufon.replace('a.information_text');
			Cufon.replace('a.featured_text');
			Cufon.replace('a.support_text');
			Cufon.replace('a.socials_text');
			Cufon.replace('.hc');
			Cufon.replace('.he');
			Cufon.replace('.hs');
			Cufon.replace('.s_tabs_nav li a');  
			
			
			
   
		


//Sidebar Link Hover

			
	



// Fancybox

$(document).ready(function(){
	
	
	//related hover
	
	      $(".related_item.one .related_product_title").hover(function()
                {
                    $(".related_item.one img").css("border", "4px solid #c9c9c7");
                }, function()
                {
                    $(".related_item.one img").css("border", "4px solid #f3f3f3");
                });
				
				$(".related_item.one img").hover(function()
                {
                    $(".related_item.one img").css("border", "4px solid #c9c9c7");
                }, function()
                {
                    $(".related_item.one img").css("border", "4px solid #f3f3f3");
                });
				
				        $(".related_item.last.two .related_product_title").hover(function()
                {
                    $(".related_item.last.two img").css("border", "4px solid #c9c9c7");
                }, function()
                {
                    $(".related_item.last.two img").css("border", "4px solid #f3f3f3");
                });
				
				$(".related_item.last.two img").hover(function()
                {
                    $(".related_item.last.two img").css("border", "4px solid #c9c9c7");
                }, function()
                {
                    $(".related_item.last.two img").css("border", "4px solid #f3f3f3");
                });
				
				
				
				        $(".related_item.three .related_product_title").hover(function()
                {
                    $(".related_item.three img").css("border", "4px solid #c9c9c7");
                }, function()
                {
                    $(".related_item.three img").css("border", "4px solid #f3f3f3");
                });
				
				$(".related_item.three img").hover(function()
                {
                    $(".related_item.three img").css("border", "4px solid #c9c9c7");
                }, function()
                {
                    $(".related_item.three img").css("border", "4px solid #f3f3f3");
                });
				
				
				
				        $(".related_item.last.four .related_product_title").hover(function()
                {
                    $(".related_item.last.four img").css("border", "4px solid #c9c9c7");
                }, function()
                {
                    $(".related_item.last.four img").css("border", "4px solid #f3f3f3");
                });
				
				$(".related_item.last.four img").hover(function()
                {
                    $(".related_item.last.four img").css("border", "4px solid #c9c9c7");
                }, function()
                {
                    $(".related_item.last.four img").css("border", "4px solid #f3f3f3");
                });
	       		// Information and meta hover
			var time_on = 100;
			var time_off = 150;
			$( '.information li a' ).hover( function() {
				$( this ).animate( { marginLeft: '9px' }, time_on );
			}, function() {
				$( this ).animate( { marginLeft: '0px' }, time_off );
			} );
			
			
			$( '.information li a' ).hover( function() {
				$( this ).animate( { paddingLeft: '9px' }, time_on );
			}, function() {
				$( this ).animate( { paddingLeft: '0px' }, time_off );
			} );
			
			
			$( '.socials  .socials_title' ).hover( function() {
				$( this ).animate( { paddingLeft: '9px' }, time_on );
			}, function() {
				$( this ).animate( { paddingLeft: '0px' }, time_off );
			} );
			





//Fancybox for image gallery
$(".simple_image").fancybox({
		'opacity'		: true,
		'overlayShow'	       : true,
		'overlayColor': '#000000',
		'overlayOpacity'     : 0.9,
		'titleShow':true,
		'transitionIn'	: 'elastic',
		'transitionOut'	: 'elastic'
});



//Gallery - On Hover Event 
	$('.simple_image').mouseenter(function(e) {
            $(this).children('img').animate(300);
            $(this).children('.zoom').fadeIn(400);
        }).mouseleave(function(e) {
            $(this).children('img').animate(300);
            $(this).children('.zoom').fadeOut(400);
});


	$('.simple_image').mouseenter(function(e) {
            $(this).children('img').animate(300);
            $(this).children('.category_zoom').fadeIn(400);
        }).mouseleave(function(e) {
            $(this).children('img').animate(300);
            $(this).children('.category_zoom').fadeOut(400);
});


	$('.simple_image').mouseenter(function(e) {
            $(this).children('img').animate(300);
            $(this).children('.product_zoom').fadeIn(400);
        }).mouseleave(function(e) {
            $(this).children('img').animate(300);
            $(this).children('.product_zoom').fadeOut(400);
});


//Category Menu Dropdown

$("#category_menu").superfish({animation:{opacity:"show",height:"show"},speed:100,delay:100,dropShadows:false,autoArrows:false,disableHI:true});

// Animation for category page select

  $('.s_switcher').hover(function() {
    $(this).find('.s_options').stop(true, true).slideDown('fast');
  },function() {
    $(this).find('.s_options').stop(true, true).slideUp('fast');
  });
  
  
    $('.cat_opt_button_container').hover(function() {
    $(this).find('.o_options').stop(true, true).slideDown('fast');
  },function() {
    $(this).find('.o_options').stop(true, true).slideUp('fast');
  });







//selectbox replace


		if (!$.browser.opera) {
    
			// select element styling
			$('select.select2').each(function(){
				var title = $(this).attr('title');
				if( $('option:selected', this).val() != ''  ) title = $('option:selected',this).text();
				$(this)
					.css({'z-index':10,'opacity':0,'-khtml-appearance':'none'})
					.after('<span class="select2">' + title + '</span>')
					.change(function(){
						val = $('option:selected',this).text();
						$(this).next().text(val);
						})
			});

		};
		
		
			if (!$.browser.opera) {
    
			// select element styling
			$('select.select').each(function(){
				var title = $(this).attr('title');
				if( $('option:selected', this).val() != ''  ) title = $('option:selected',this).text();
				$(this)
					.css({'z-index':10,'opacity':0,'-khtml-appearance':'none'})
					.after('<span class="select">' + title + '</span>')
					.change(function(){
						val = $('option:selected',this).text();
						$(this).next().text(val);
						})
			});

		};
		
	
//checkbox replace

	//$(".radio").dgStyle();
	//$(".checkbox2").dgStyle();



	//$(".radio").dgStyle();
	//$(".checkbox").dgStyle();



});



   
	
		
	
		
	



