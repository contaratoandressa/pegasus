import cv2
from skimage import io
import numpy as np

def fc_quant_color(file, n):

  df = np.float32(file).reshape((-1, 3))

  c = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 15, 0.002)

  ret, label, center = cv2.kmeans(df, n, None, c, 20, cv2.KMEANS_RANDOM_CENTERS)
  center = np.uint8(center)
  final_result = center[label.flatten()]
  final_result = final_result.reshape(file.shape)
  return final_result