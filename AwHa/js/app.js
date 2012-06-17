var weekDays=["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"];
var habits=new Array();
habits.push("Drink at least 10 glasses of water every day");
habits.push("Eat a healthy diet");
habits.push("Spend at least 15 minutes a day learning a new skill");
habits.push("Be punctual");
habits.push("Brisk-walk at least 30 minutes a day");
habits.push("Brush teeth twice a day");
habits.push("Refrain from watching TV");
var dailyHabits=new Array();
for (var di = 0; di < 7; di++) {
  dailyHabits[di]=new Array();
  for (var hi = 0; hi < habits.length; hi++) {
    dailyHabits[di][hi]=0;
  }
}
$(".pretend").click( function(e) { e.preventDefault(); currentDay=parseInt($(this).attr("id")); $(".visualstate").hide( "fast", function() { rewriteEachDayDivFromScratch(); $("#eachday").show(); } ); } );
$("#gohome").click( function(e) { e.preventDefault(); $(".visualstate").hide( "fast", function() { $("#welcomescreen").show(); } ); } );
function rewriteEachDayDivFromScratch()
{
  $("#eachday").html("");
  $("#eachday").append("<h2>Well, it's time to check in.</h2>");
  $("#eachday").append("<h3>Do this at the end of each day.</h3>");
  $("#eachday").append("<br>");
  var todaysHabits=dailyHabits[currentDay];
  for (var hi = 0; hi < todaysHabits.length; hi++) {
    notchecked_id='notchecked_'+hi;
    checked_id='checked_'+hi;
    var txt='';
    txt+='<div id="'+notchecked_id+'" class="notchecked" style="display: none;">';
    txt+='<button class="btn btn-mini btn-inverse">☐ '+habits[hi]+'</button>';
    txt+='</div>';
    txt+='<div id="'+checked_id+'" class="checked" style="display: none;">';
    txt+='<button class="btn btn-mini">☑ '+habits[hi]+'</button>';
    txt+='</div>';
    txt+='<br>';
    $("#eachday").append(txt);
    if (dailyHabits[currentDay][hi] == 0)
    {
      $("#"+notchecked_id).show();
    }
    else
    {
      $("#"+checked_id).show();
    }
  }
  $(".notchecked").click( function(e) { e.preventDefault(); var currentHabit=parseInt($(this).attr("id").replace(/^notchecked_/, "")); dailyHabits[currentDay][currentHabit]=1; $("#notchecked_"+currentHabit).hide(); $("#checked_"+currentHabit).show(); } );
  $(".checked").click( function(e) { e.preventDefault(); var currentHabit=parseInt($(this).attr("id").replace(/^checked_/, "")); dailyHabits[currentDay][currentHabit]=0; $("#checked_"+currentHabit).hide(); $("#notchecked_"+currentHabit).show(); } );
}

