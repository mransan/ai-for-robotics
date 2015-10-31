

function min_max(d, accessor) {
  if(d.length === 0) {
    return undefined; 
  }
  var min_value = accessor(d[0]);
  var max_value = min_value; 
  d.forEach(function(x) {
    var x = accessor(x); 
    if(x > max_value) {
      max_value = x; 
    }
    else if (x < min_value) {
      min_value = x; 
    }
  }); 

  return [min_value  , max_value  ];
}
