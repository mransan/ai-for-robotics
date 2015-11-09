function Distribution(config) {

  var self = this;

  var width  = config.width; 
  var height = config.height; 
  var d3_svg = config.d3_svg; 
  var title   = config.title;
  var title_class = config.title_class;

  d3_svg.attr("width"  , width);
  d3_svg.attr("height" , height);

  var margin = config.margin; 
  if(margin === undefined) {
    margin = {
      top:50, 
      right:20, 
      bottom:20, 
      left:50
    }; 
  }
  
  var width  = width  - margin.left - margin.right; 
  var height = height - margin.top - margin.bottom;

  var x = d3.scale.linear().range([0, width]);
  var y = d3.scale.linear().range([height, 0]);

  var x_axis_selection = d3_svg.append("g")
    .attr("class", "x axis")
    .attr("transform", svg_translate(margin.left, height + margin.top ))
  
  var y_axis_selection = d3_svg.append("g")
    .attr("class", "y axis")
    .attr("transform", svg_translate(margin.left - 1, margin.top))

  var text_selection = d3_svg.append("text")
      .attr("x", 40)
      .attr("y", 20)
      .text(title); 
  if(title_class !== undefined) {
      text_selection.attr("class", title_class);
  }
  
  self.display = function(data) {
    
    var x_increment = data[1][0] - data[0][0]; 

    x.domain([data[0][0]- x_increment/2., 
              data[data.length - 1][0] + x_increment/2.]); 
    y.domain([0, d3.max(data, function(d) { return d[1]; } )]); 

    var x_increment = x(data[1][0]) - x(data[0][0]); 

    function set_common(e) {
      return e
      .attr("x", function(d) {
        return margin.left + x(d[0]) - (x_increment/2.); 
      })
      .attr("y", function(d) { 
        return margin.top + y(d[1]); 
      })
      .attr("height", function(d) { 
        return height - y(d[1]); 
      })
      .attr("width", x_increment - 1.)
    }
    
    var join_update = d3_svg.selectAll(".bar").data(data); 
    set_common(join_update); 
    var join_add = join_update.enter().append("rect"); 
    set_common(join_add);
    join_add.attr("class", "bar");

    var xAxis   = d3.svg.axis().scale(x).orient("bottom"); 
    x_axis_selection.call(xAxis);
      
    var yAxis = d3.svg.axis().scale(y).orient("left");
    y_axis_selection.call(yAxis);
  };
}
