$(".cdlmenuitem").on("click", function(e) {
  e.preventDefault();
  $(".visualstate").hide();
  var id_of_click_target = $(this).attr("id");
  var css_selector_to_display = "#" + id_of_click_target + "_content";

  if (css_selector_to_display === "#calendar_content") {
    var o = window.orientation;
    if (o) {
      if (o === 90 || o === -90) {
        $("#calendar_iframe").attr("width", "100%");
      } else {
        if (window.screen.width <= 320) {
          $("#calendar_iframe").attr("width", "275");
        }
      }
      $(window).trigger("resize");
    } else {
      $("#calendar_iframe").attr("width", "100%");
    }
  }

  $(css_selector_to_display).css({"display": "block", "opacity": 0}).animate({"opacity": 1}, 250);
  history.pushState(css_selector_to_display, "/", window.location.href);
  if ($("#menu").css("display") === "none") {
    $("#cdl_heading").hide();
    document.getElementById("top_of_content").scrollIntoView();
  } else {
    $("#cdl_heading").show();
    document.body.scrollIntoView();
  }
});

$("#menu_header").on("click", function(e) {
  e.preventDefault();
  $(".visualstate").hide();
  $("#cdl_heading").show();
  $("#acasa_content").css({"display": "block", "opacity": 0}).animate({"opacity": 1}, 250);
  history.pushState("#acasa_content", "/", window.location.href);
  document.body.scrollIntoView();
});

$(window).on("popstate", function(event) {
  if (typeof event.state === "string") {
    $(".visualstate").hide();
    $(event.state).css({"display": "block", "opacity": 0}).animate({"opacity": 1}, 250);
    if ($("#menu").css("display") === "none") {
      $("#cdl_heading").hide();
    } else {
      $("#cdl_heading").show();
    }
  };
});

$(window).on("orientationchange", function(event) {
  var o = window.orientation;
  if (o === 90 || o === -90) {
    $("#calendar_iframe").attr("width", "100%");
  } else {
    if (window.screen.width <= 320) {
      $("#calendar_iframe").attr("width", "275");
    }
  }
  $(window).trigger("resize");
});

Zepto(function($) {
  history.pushState("#acasa_content", "/", window.location.href);
  var all_images = {
    "#cdl_logo":       "images/cdl.png",
    "#rosedu_logo":    "images/rosedu.png",
    "#ixia_logo":      "images/ixia.png",
    "#eaudeweb_logo":  "images/eaudeweb.png",
    "#google_logo":    "images/google.png",
    "#english_logo":   "images/english.png",
    "#arrow_hover":    "images/arrow-hover.png",
    "#green_gradient": "images/green-gradient.png",
    "#alexef_pic":     "images/alexef.png"
  }
  for (var img_id in all_images) {
    $(img_id).attr("src", all_images[img_id]);
  }

  var iframe_url = "http://www.google.com/calendar/embed?src=apve67v2o1l4tp1655sl53nhs8%40group.calendar.google.com&ctz=Europe/Bucharest&bgcolor=%23F3F3F3"
  $("#calendar_iframe").attr("src", iframe_url);

  $("#calendar_iframe").attr("width", "275");
});

