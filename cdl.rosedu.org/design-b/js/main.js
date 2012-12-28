$(".cdlmenuitem").on("click", function(e) {
  e.preventDefault();
  $(".visualstate").hide();
  var id_of_click_target = $(this).attr("id");
  var css_selector_to_display = "#" + id_of_click_target + "_content";
  $(css_selector_to_display).css({"display": "block", "opacity": 0}).animate({"opacity": 1}, 250);
  history.pushState(css_selector_to_display, "/", window.location.href);
});

$("#menu_header").on("click", function(e) {
  e.preventDefault();
  $(".visualstate").hide();
  $("#acasa_content").css({"display": "block", "opacity": 0}).animate({"opacity": 1}, 250);
  history.pushState("#acasa_content", "/", window.location.href);
});

window.addEventListener('popstate', function(event) {
  if (typeof event.state === "string") {
    $(".visualstate").hide();
    $(event.state).css({"display": "block", "opacity": 0}).animate({"opacity": 1}, 250);
  };
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
});

