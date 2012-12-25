$(".cdlmenuitem").on("click", function(e) {
  e.preventDefault();
  $(".visualstate").hide();
  id_of_click_target = $(this).attr("id");
  css_selector_to_display = "#" + id_of_click_target + "_content";
  $(css_selector_to_display).show();
  history.pushState(css_selector_to_display, "/", window.location.href);
});

$(".brand").on("click", function(e) {
  e.preventDefault();
  $(".visualstate").hide();
  $("#acasa_content").show();
  history.pushState("#acasa_content", "/", window.location.href);
});

window.addEventListener('popstate', function(event) {
  if (typeof event.state === "string") {
    $(".visualstate").hide();
    $(event.state).show();
  };
});

Zepto(function($) {
  history.pushState("#acasa_content", "/", window.location.href);
});

