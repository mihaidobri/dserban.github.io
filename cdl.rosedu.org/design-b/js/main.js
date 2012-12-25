$(".cdlmenuitem").on("click", function(e) {
  e.preventDefault();
  $(".visualstate").hide();
  id_of_click_target = $(this).attr("id");
  css_selector_to_display = "#" + id_of_click_target + "_content";
  $(css_selector_to_display).show();
  history.pushState(css_selector_to_display, "/", window.location.href);
});

$("#menu_header").on("click", function(e) {
  e.preventDefault();
  $(".visualstate").hide();
  $("#acasa_content").show();
  history.pushState("#acasa_content", "/", window.location.href);
});

$("#calendar_iframe_resizer").on("click", function(e) {
  e.preventDefault();
  $("#calendar_iframe").attr("width", 800);
});

window.addEventListener('popstate', function(event) {
  if (typeof event.state === "string") {
    $(".visualstate").hide();
    $(event.state).show();
  };
});

Zepto(function($) {
  history.pushState("#acasa_content", "/", window.location.href);
  all_images = {
    "#cdl_logo":      "images/cdl.png",
    "#rosedu_logo":   "images/rosedu.png",
    "#ixia_logo":     "images/ixia.png",
    "#eaudeweb_logo": "images/eaudeweb.png",
    "#google_logo":   "images/google.png",
    "#english_logo":  "images/english.png",
    "#alexef_pic":    "images/alexef.png"
  }
  for (var img_id in all_images) {
    $(img_id).attr("src", all_images[img_id]);
  }
});

