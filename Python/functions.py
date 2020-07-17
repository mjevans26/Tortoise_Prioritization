def normalize(array):
  """ Normalize array to [0, 1]
  """
  minimum = np.nanmin(array)
  maximum = np.nanmax(array)
  y = maximum - array/(maximum - minimum)
  return(y)

def relu(array, b):
    """ 
    Perform rectified linear unit transformation on array
    Args:
      b (int): slope parameter of linear rescaling
    Return:
      ndarray: transformed values
    """
    y = array*b
    return np.where(y>1, 1, y)
        
def sigmoid(array, x0, b):
    """Sigmoid function

    Args:
      array (ndarray): 2D array of values to be transformed
      x0 (int): inflection point
      b (int): scale parameter

    Returns:
      ndarray: image containing transformed values
    """
    y = 1/(1 + np.exp(-b*(array - x0)))
    return(y) 

def logarithmic(array, b):
  """Log-like function bounded [0,1]

  1-exp(-3*x)

  Args:
    array (ndarray): 2D array of values to be transformed
    b (int): scale parameter

  Returns:
    ndarray: log transformed values
  """  
  y = 1 - np.exp(-b*array)
  return(y)

def exponential(array, b):
  y = array**b 
  return(y)

    
def block(raster, weight, mode):
  """recalculate a raster given a transformation and scale
  Args:
  	raster (ndarray): 2D array of values to be transformed
	weight (int): scaling parameter
	mode (str): Name of transformation function to apply
  Returns:
  	ndarray: transformed output of same shape and size as input
  """
  if(mode == 'Linear'):
      out = relu(raster, weight)
  elif(mode == 'Exponential'):
      out = raster.pow(weight)
  elif(mode == 'Sigmoid'):
      out = sigmoid(raster, 0.5, weight*10)
  elif(mode == 'Logarithmic'):
      out = logarithmic(raster, weight)
  else:
      out = raster
  return out

class rasterCalculator:
  def __init__(self, rasters, modes, weights):
    self.rasters = rasters
    self.modes = modes
    self.weights = weights
    self.n = len(rasters)

  def calculate(self):
  	""" 
	transform and combine one or more raster(s) using weight(s) and function(s)
	"""
    combined = sum([block(self.rasters[x], self.weights[x], self.modes[x]) for x in range(self.n)])
    return combined