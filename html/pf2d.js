function PF2D (config) {

  var self = this; 

  // Mandatory
  // ----

  var height = config.height; 
  var width = config.width; 
  var d3_svg = config.d3_svg; 
  var particle_class = config.particle_class; 
  var truth_class    = config.truth_class; 
  var theta_class    = config.theta_class; 
  
  // Optional
  // ----
  
  var margin = config.margin; 
  if(margin === undefined) {
      margin = {top:0, bottom:0, left:0, right:0}; 
  }
  var radius = config.radius; 
  if(radius === undefined) {
      radius = 7;
  }

  // Reusable variable for each time stamp. 
  //

  d3_svg.attr("width", width) .attr("height", height); 

  var x = d3.scale.linear().range([margin.left , width -margin.right]);
  var y = d3.scale.linear().range([height - margin.top, margin.bottom]);
  
  
  // each robot position is of the form 
  // [x, y, theta, is_truth]
  //
  var x_index = 0; 
  var y_index = 1;
  var theta_index = 2; 
  var truth_index = 3;
  
  function get_x(d) {return d[x_index];} 
  function get_y(d) {return d[y_index];}

  var particles_g = d3_svg.append("g"); 


  

  self.setup = function(d, accessor) {
    var max_x = d3.max(d, function(d) { return d3.max(accessor(d), get_x); });
    var max_y = d3.max(d, function(d) { return d3.max(accessor(d), get_y); });
    var min_x = d3.min(d, function(d) { return d3.min(accessor(d), get_x); });
    var min_y = d3.min(d, function(d) { return d3.min(accessor(d), get_y); });
    
    x.domain([min_x, max_x]);
    y.domain([min_y, max_y]);
  
    //var chart = chart.append("g").attr("transform", "translate(" + margin.left + ", " + margin.top + ")");
    
    var y_axis = d3.svg.axis().scale(y).orient("left");
    d3_svg.append("g")
      .attr("class", "y axis")
      .attr("transform", "translate(" + 30 + ", 0)")
      .call(y_axis);
    
    var x_axis   = d3.svg.axis().scale(x).orient("bottom"); 
    d3_svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + (height - 60) + ")")
      .call(x_axis);
  };
  
  self.display = function (data) {
    var circles = particles_g.selectAll("circle"); 
    var lines   = particles_g.selectAll("line");

     circles.data(data)
      .attr("cx", function(d) { return x(d[x_index]); }) 
      .attr("cy", function(d) { return y(d[y_index]); }) 
      .enter().append("circle")
        .attr("cx", function(d) { return x(d[x_index]) ; })
        .attr("cy", function(d) { return y(d[y_index]) ; })
        .attr("r" , function(d) { return radius; })
        .attr("class", function(d) { 
            if(d[truth_index]) {
                var class_ = truth_class;
            }
            else {
                var class_ = particle_class;
            }
            return class_;
        });

    lines.data(data)
      .attr("x1", function(d) { return x(d[x_index]); })
      .attr("y1", function(d) { return y(d[y_index]); })
      .attr("x2", function(d) { return x(d[x_index]) + radius * Math.cos(d[theta_index]); }) 
      .attr("y2", function(d) { return y(d[y_index]) - radius * Math.sin(d[theta_index]); }) 
      .enter().append("line")
        .attr("x1", function(d) { return x(d[x_index]);})
        .attr("y1", function(d) { return y(d[y_index]);})
        .attr("x2", function(d) { return x(d[x_index]) + radius * Math.cos(d[theta_index]); }) 
        .attr("y2", function(d) { return y(d[y_index]) - radius * Math.sin(d[theta_index]); })
        .attr("class", theta_class);
  };
};
