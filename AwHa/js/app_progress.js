/* track progress */
var TrackModel = Backbone.Model.extend({
  initialize: function() {
  if (!this.get("name")){
    this.set({"name":"habit"}); 
    this.set({"progress":32});
    this.set({"data": [0]})
  }
  if(null == this.get("data") || this.get("data").length > 7){
    throw new Error("invalid data");
  }
  var progressSum = _.reduce(this.get("data"), function(memo, num){return memo + num;});
  this.set({progress:(progressSum*100/7).toFixed(2)});
  }
});


$(function(){
  var TrackProgressEntryView = Backbone.View.extend({
    tagName:  "tr",
    className: "hero-entry",
    template: _.template($('#progress-template').length?$('#progress-template').html():'<td><%= name %></td><td><%= progress %>%</td>'),
    events: {},
    initialize: function() {
      //
    },

    render: function() {
      this.$el.html(this.template(this.model.toJSON()));
      return this;
    }
  });

  var TrackProgressView = Backbone.View.extend({
    el: $('#trackprogress'),

    events: {
      "click #progress-link": "show"
    },

    initialize: function() {
      $('#progress-link').bind('click', this.show);
      this.$el.hide();
    },

    show: function(e){
      e.preventDefault()
      console.log("in show");
      window.track.render();
      $(".visualstate").hide().promise().done( function() { 
        $('#trackprogress').show();
      });
      
    },    
    //keep data when model is available & bind
    render: function(){
      var tbody = this.$el.find('tbody');
      tbody.children().remove()
      
      var allData = window.dailyHabits
      var habits = window.habits    
      for (var i = 0;i<habits.length;i++){
        var progressData = _.map(allData, function(dataForADay){return dataForADay[i];});
        var tm = new TrackModel({name: habits[i], data: progressData});
        var entryView = new TrackProgressEntryView({model: tm});
        tbody.append(entryView.render().el)        
      }

      return this.el;
    }
  });

  var trackview = new TrackProgressView;
  trackview.render();
  window.track = trackview;
});
