var suitability = ee.Image("users/defendersofwildlifeGIS/Tortoise/suitability"),
    connectivity = ee.Image("users/defendersofwildlifeGIS/Tortoise/connectivity"),
    counties = ee.FeatureCollection("TIGER/2018/Counties"),
    clarkGain = ee.FeatureCollection("users/defendersofwildlifeGIS/Tortoise/clarkGain"),
    clarkLoss = ee.FeatureCollection("users/defendersofwildlifeGIS/Tortoise/clarkLoss");
var clarkCounty = counties.filterMetadata('GEOID', 'equals', '32003')
connectivity = connectivity.clip(clarkCounty)
suitability = suitability.clip(clarkCounty)

var meansum = ee.Reducer.mean().combine({
  reducer2: ee.Reducer.sum(),
    sharedInputs:true
  });
  
function normalize(img, aoi){
// Rescale image from 0 to 1
//
//  Args:
//    img (ee.Image): input raster to rescale
//
//  Returns:
//    ee.Image: normalized image

var minmax = img.reduceRegion({
      reducer:ee.Reducer.min().combine({
        reducer2: ee.Reducer.max(),
        sharedInputs:true}),
      geometry:aoi,
      scale:30,
      maxPixels:1e13,
      tileScale:8
  }).toImage()

  var minimum = minmax.select('b1_min')
  var maximum = minmax.select('b1_max')

  return ee.Image(img).subtract(minimum).divide(maximum.subtract(minimum))
}

function relu(img, b){
//  """Rectified linear unit
//    
//  Args:
//    img (ee.Image): input raster to be transformed
//    b (num): slope of linear scaling
//
//  Returns:
//    ee.Image: image containing transformed values

  var out = ee.Image(img).multiply(b)
  var one = ee.Image.constant(1)
  return out.where(out.gte(1), one)
}

function sigmoid(img, x0, k){
//  Sigmoid function
//
//  y = 1/(1 + np.exp(-k*(x - x0)))
//  Args:
//    img (ee.Image): input raster to be transformed
//    x0 (int): inflection point
//    k (int): scale parameter
//
//  Returns:
//    ee.Image: image containing transformed values

    var y = ee.Image(img).subtract(x0).multiply(-k).exp().add(1).pow(-1)

    return(y)
}

function logarithm(img, b){
//  Log-like function bounded [0,1]
//
//  1-exp(-3*x)
//
//  Args:
//    img (ee.Image): 
//    b (int): 
//
//  Returns:
//    ee.Image: transformed image
  var y = ee.Image(img).multiply(-b).exp().multiply(-1).add(1)
  return(y)
}

function calc_value(suit_rast, conn_rast, suit, conn, suitMode, connMode){
//    Transform and combine two rasters
//    
//    Args:
//      suit_rast (ee.Image): raster representing habitat suitability
//      conn_rast (ee.Image): raster representing habitat conectivity
//      suit (int): weight for suitability value functions
//      conn (int): weight for connectivity value functions
//      suitMode (str): specifying suitability value function
//      connMode (str): specifying connectivity value funtion
//      
//    Returns:
//      ee.Image: image containing transformed and combined values

    suit_rast = ee.Image(suit_rast)
    conn_rast = ee.Image(conn_rast)
    
    var suitable = ee.Algorithms.If(suitMode == 'Linear',
                                       relu(suit_rast, suit),
                                       ee.Algorithms.If(suitMode == 'Exponential',
                                       suit_rast.pow(suit),
                                       ee.Algorithms.If(suitMode == 'Sigmoid',
                                       sigmoid(suit_rast, 0.5, suit*10),
                                       ee.Algorithms.If(suitMode == 'Logarithmic',
                                       logarithm(suit_rast, suit),
                                       suit_rast))))
                                       
    var connect = ee.Algorithms.If(connMode == 'Linear',
                                       relu(conn_rast, conn),
                                       ee.Algorithms.If(connMode == 'Exponential',
                                       conn_rast.pow(conn),
                                       ee.Algorithms.If(connMode == 'Sigmoid',
                                       sigmoid(conn_rast, 0.5, conn*10),
                                       ee.Algorithms.If(connMode == 'Logarithmic',
                                       logarithm(conn_rast, conn),
                                       conn_rast))))
        
    var value = ee.Image(connect).add(suitable)

    return value
}

function calc_value_num(value, weight, mode){
//    Transform a scalar using one of several functions
//    
//    Args:
//      value (ee.Number): habitat suitability/connectivity value
//      weight (int): weight for value functions
//      mode (str): specifying value function
//      
//    Returns:
//      ee.Number: transformed value

    value = ee.Number(value)
    
    var outVal = ee.Algorithms.If(mode == 'Linear',
                                       value.multiply(weight).min(1),
                                       ee.Algorithms.If(mode == 'Exponential',
                                       value.pow(weight),
                                       ee.Algorithms.If(mode == 'Sigmoid',
                                       value.subtract(0.5).multiply(-weight*10).exp().add(1).pow(-1),
                                       ee.Algorithms.If(mode == 'Logarithmic',
                                       value.multiply(-weight).exp().multiply(-1).add(1),
                                       value))))
                                       
    outVal = ee.Number(outVal)

    return outVal
}

function zonal_stats(values, ftCol, property){
//  Calculate zonal statistics of a raster within polygons
//  
//  Calculates the mean and sum of raster values within the polygons of a feature
//  collection. Optionally, if the 'property' argument is specified, will
//  calculate these statistics per subset of features as determined by the unique
//  values of the property
//
//  Args:
//    values (ee.Image): image of values to summarize
//    ftCol (ee.FeatureCollection): feature(s) within which stats are calculated
//    property (str): property name to be used to categorize features
//
//  Returns:
//    ee.Dictionary:

  function stats_by_property(i, dict){
    var names = ['mean_', 'sum_'].map(function(str){return (ee.String(str).cat(i))});
    var stats = values.reduceRegion({
      reducer: meansum,
      geometry: ftCol.filterMetadata(property, 'equals', i),
      scale: 30,
      maxPixels: 1e13,
      tileScale: 6
    }).rename(['b1_mean', 'b1_sum'], names);
    return ee.Dictionary(dict).combine(stats);
  }
  
  if(property === ''){
    var output = values.reduceRegion({
      reducer: meansum,
      geometry: ftCol,
      scale: 30,
      maxPixels: 1e13,
      tileScale: 6
    });
  }else{
    var first = ee.Dictionary({});
    var properties = ee.List(ftCol.aggregate_array(property)).distinct();
    var output = properties.iterate(stats_by_property, first);
  }
  /*
  var lossValue = values.reduceRegion({
    reducer:ee.Reducer.mean().combine({
      reducer2: ee.Reducer.sum(),
      sharedInputs:true}),
      geometry:ftCol.filterMetadata(property, 'equals', i),
      scale:30,
      maxPixels:1e13,
      tileScale:6}).rename(['b1_mean', 'b1_sum'], ['mean_loss', 'sum_loss'])
      
  var gainValue = values.reduceRegion({
    reducer:ee.Reducer.mean().combine({
      reducer2: ee.Reducer.sum(),
      sharedInputs:true}),
      geometry:gain,
      scale:30,
      maxPixels:1e13,
      tileScale:6}).rename(['b1_mean', 'b1_sum'], ['mean_gain', 'sum_gain'])
  
  return gainValue.combine(lossValue)
  */
  return output
}

// Rescale the connectivity raster to [0, 1]
var connRescale = normalize(connectivity, clarkCounty)

// Define viridis visualization parameters for value surface
var vizParams = {'palette': ["440154FF", "3B528BFF", "21908CFF", "5DC863FF", "FDE725FF"], 'min': 0, 'max': 2}

// Add initial value surface to the map
var map = ui.Map({lat:36.111, lon:-114.871, zoom:8});
var value = calc_value(suitability, connRescale, 1, 1, 'Linear', 'Linear');
map.addLayer(value, vizParams, 'value');
map.addLayer(clarkGain, {'color':'orange'}, 'Gains');
map.addLayer(clarkLoss, {'color': 'grey'}, 'Losees');

// UI
// Create charts to display valuation functions
var xaxis = ee.List.sequence(0, 1, 0.01);

var yaxis = xaxis.map(function(x){
  return calc_value_num(x, 1, 'Linear');
});

function make_chart(b, mode){
//    Create demonstration weighting function charts
//    
//    Args:
//      b (ee.Number): function weight
//      mode (str): specifying value function
//      
//    Returns:
//      ui.Chart: scatter plot showing transformation over 0 to 1

  var yaxis = xaxis.map(function(x){
    return calc_value_num(x, b, mode)
  })
  var chart = ui.Chart.array.values({
    array: ee.Array(yaxis),
    axis: 0,
    xLabels: xaxis
  })
  .setOptions({
    title: '',
    hAxis: {'title': 'Suitability'},
    vAxis: {'title': 'Value'},
    pointSize: 3
    });
  
  chart.style().set({height: '100px', width: '250px', margin: 'auto'});
  return chart
}

// Make initial charts
var suitChart = make_chart(1, 'Linear')
var connChart = make_chart(1, 'Linear')

// Create an intro modal
var title = ui.Label('Tortoise Habitat Valuation',{
  'font-weight': 'bold',
  'font-size': '18px',
  'margin':'auto'
})

var instructions1 = ui.Label ('The Southern Nevada Economic Development and Conservation Act proposes to turn existing protected federal lands over to Clark county in excahnge for creating new conservation areas in others.');
var instructions2 = ui.Label('Clark county lies within a portion of the range of the endangered Mojave desert tortoise.');
var instructions3 = ui.Label('To understand the impact of proposed changes to tortoise habitat, this app lets users choose how they value habitat connectivity and suitability. After choosing a valuation scheme, users can calculate the "value" of gains and loss areas under the bill.');
var gotItButton = ui.Button('Got It!')

var introPanel = ui.Panel({
  widgets: [title, instructions1, instructions2, instructions3, gotItButton],
  style: {'border': "1.5px solid black",
          'text-align': 'center',
          'position': 'top-center',
          'width': '600px',
          'height': '300px'
  },
  layout: ui.Panel.Layout.flow('vertical')
});

gotItButton.onClick(function(){map.remove(introPanel)});

// Define common input widget parameters
var sliderParams = {min: 1, max: 5, value: 1, step: 1, style: {margin:'auto'}}
var selectParams = {
  items:['Exponential', 'Linear', 'Logarithmic', 'Sigmoid'],
  placeholder: 'Select the form of the function',
  value: 'Linear',
  style: {'margin':'auto'}
}

// Create input widgets
var connSlider = ui.Slider(sliderParams)
var suitSlider = ui.Slider(sliderParams)
var connSelect = ui.Select(selectParams)
var suitSelect = ui.Select(selectParams)

// Create various labels
var parameters = ui.Label('1. Valuation Parameters', {fontSize:'14px', fontWeight:'bold'})
var connlabel = ui.Label('Connectivity Function', {fontSize: '12px', fontWeight:'bold'})
var connweight = ui.Label('Connectivity Weight', {fontSize: '12px', fontWeight: 'bold'})
var suitlabel = ui.Label('Suitability Function', {fontSize: '12px', fontWeight: 'bold'})
var suitweight = ui.Label('Suitability Weight', {fontSize: '12px', fontWeight: 'bold'})

// Create sub-panels for finer UI display control
var connSelectPanel = ui.Panel({
  widgets: [connlabel, connSelect],
  layout: ui.Panel.Layout.flow('vertical')
})
var connSliderPanel = ui.Panel({
  widgets: [connweight, connSlider],
  layout: ui.Panel.Layout.flow('vertical')
})
var suitSelectPanel = ui.Panel({
  widgets: [suitlabel, suitSelect],
  layout: ui.Panel.Layout.flow('vertical')
})
var suitSliderPanel = ui.Panel({
  widgets: [suitweight, suitSlider],
  layout: ui.Panel.Layout.flow('vertical')
})

// place the each two controls connectivity and suitability side-by-side
var connPanel = ui.Panel({
  widgets: [connSelectPanel, connSliderPanel],
  layout: ui.Panel.Layout.flow('horizontal')
})
var suitPanel = ui.Panel({
  widgets: [suitSelectPanel, suitSliderPanel],
  layout: ui.Panel.Layout.flow('horizontal')
})

// add subpanels to main input panel
var inputPanel = ui.Panel([parameters, connPanel, connChart, suitPanel, suitChart],
  ui.Panel.Layout.flow('vertical'),
  {'width':'300px'})

// Define behavior for each input
connSlider.onChange(function(conn){
  var suit = suitSlider.getValue()
  var suitMode = suitSelect.getValue()
  var connMode = connSelect.getValue()
  value = calc_value(suitability, connRescale, suit, conn, suitMode, connMode)
  map.layers().get(0).setEeObject(value)
  var chart = make_chart(conn, connMode)
  inputPanel.widgets().set(2, chart)
})

suitSlider.onChange(function(suit){
  var conn = connSlider.getValue()
  var suitMode = suitSelect.getValue()
  var connMode = connSelect.getValue()
  value = calc_value(suitability, connRescale, suit, conn, suitMode, connMode)
  map.layers().get(0).setEeObject(value)
  var chart = make_chart(suit, suitMode)
  inputPanel.widgets().set(4, chart)
})

connSelect.onChange(function(connMode){
  var conn = connSlider.getValue()
  var suit = suitSlider.getValue()
  var suitMode = suitSelect.getValue()
  value = calc_value(suitability, connRescale, suit, conn, suitMode, connMode)
  map.layers().get(0).setEeObject(value)
  var chart = make_chart(conn, connMode)
  inputPanel.widgets().set(2, chart)
})

suitSelect.onChange(function(suitMode){
  var suit = suitSlider.getValue();
  var conn = connSlider.getValue();
  var connMode = connSelect.getValue();
  value = calc_value(suitability, connRescale, suit, conn, suitMode, connMode)
  map.layers().get(0).setEeObject(value);
  var chart = make_chart(suit, suitMode);
  inputPanel.widgets().set(4, chart);
})

// Analysis panel elements
var smaTotal = ui.Label({
  style: {'font-size':'12px'}
});

var consTotal = ui.Label({
  style: {'font-size':'12px'}
});

var dispTotal = ui.Label({
  style: {'font-size':'12px'}
});

var recTotal = ui.Label({
  style: {'font-size':'12px'}
});

var analyze = ui.Button({
  label: 'Calculate',
  style: {'margin':'auto'},
  onClick: function(){
    var stats = zonal_stats(value, clarkGain.merge(clarkLoss), 'Type').getInfo();
    var totalSMA = Math.round(stats['sum_Proposed SMA']);
    var meanSMA = Math.round(stats['mean_Proposed SMA']*100)/100
    var totalCons = Math.round(stats['sum_Conservation Area'])
    var meanCons = Math.round(stats['mean_Conservation Area']*100)/100
    var totalRec = Math.round(stats['sum_Recreation Area']);
    var meanRec = Math.round(stats['mean_Recreation Area']*100)/100
    var totalDisp = Math.round(stats['sum_Disposal Area'])
    var meanDisp = Math.round(stats['mean_Disposal Area']*100)/100    
    smaTotal.setValue('Value of proposed SMAs = ' + totalSMA + ' (mean = ' + meanSMA + ')')
    consTotal.setValue('Value of conservation areas = ' + totalCons + ' (mean = ' + meanCons + ')')
    recTotal.setValue('Value of recreation areas = ' + totalRec + ' (mean = ' + meanRec + ')')
    dispTotal.setValue('Value of disposal areas = ' + totalDisp + ' (mean = ' + meanDisp + ')')    
  }
})

var analysisTitle = ui.Label({
  value: '3. Total gains and losses',
  style: {'position':'top-center', 'font-size':'14px', 'font-weight':'bold'}
})

var analysisPanel = ui.Panel({
  widgets: [analysisTitle, analyze, smaTotal, consTotal, recTotal, dispTotal],
  style: {'position':'bottom-right', 'height':'150px'}
})

// Define ui components to inspect individual tracts
var inspectTitle = ui.Label({
  value: '2. Inspect value of tracts (click the map)',
  style: {'position': 'top-center', 'font-size':'14px', 'font-weight':'bold'}
})

var inspectOutput = ui.Label({
  value: 'Click orange (gain) or grey (loss) polygons to see their conservation value',
  style: {'font-size':'12px', 'width':'250px'}
})

var inspectPanel = ui.Panel({
  widgets: [inspectTitle, inspectOutput],
  style: {'position':'bottom-left'}
})

map.onClick(function(coords){
  coords = ee.Geometry.Point([coords['lon'], coords['lat']])
  var subset = clarkGain.merge(clarkLoss).filterBounds(coords)
  var stats = zonal_stats(value, subset, '').getInfo()
  var type = subset.aggregate_array('Type').getInfo()[0]
  inspectOutput.setValue(type +' value = ' + Math.round(stats['b1_sum']) + ' (mean = ' + Math.round(stats['b1_mean']*100)/100 + ')')
})

ui.root.clear()
ui.root.add(inputPanel)
ui.root.add(map)
map.add(introPanel)
map.add(analysisPanel)
map.add(inspectPanel)