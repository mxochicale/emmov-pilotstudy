Description of OpenFace Data [:link:](https://github.com/TadasBaltrusaitis/OpenFace/wiki)
---

Headers of `default.txt` file


```
frame, timestamp, confidence, success, 

gaze_0_x, gaze_0_y, gaze_0_z, gaze_1_x, gaze_1_y, gaze_1_z, 

pose_Tx, pose_Ty, pose_Tz, pose_Rx, pose_Ry, pose_Rz, 

x_0, x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10, x_11, x_12, x_13, x_14, x_15, x_16, 
x_17, x_18, x_19, x_20, x_21, 
x_22, x_23, x_24, x_25, x_26, 
x_27, x_28, x_29, x_30, 
x_31, x_32, x_33, x_34, x_35, 
x_36, x_37, x_38, x_39, x_40, x_41, 
x_42, x_43, x_44, x_45, x_46, x_47, 
x_48, x_49, x_50, x_51, x_52, x_53, x_54, x_55, x_56, x_57, x_58, x_59, x_60, x_61, x_62, x_63, x_64, x_65, x_66, x_67, 

y_0, y_1, y_2, y_3, y_4, y_5, y_6, y_7, y_8, y_9, y_10, y_11, y_12, y_13, y_14, y_15, y_16, 
y_17, y_18, y_19, y_20, y_21, 
y_22, y_23, y_24, y_25, y_26, 
y_27, y_28, y_29, y_30, 
y_31, y_32, y_33, y_34, y_35, 
y_36, y_37, y_38, y_39, y_40, y_41, 
y_42, y_43, y_44, y_45, y_46, y_47, 
y_48, y_49, y_50, y_51, y_52, y_53, y_54, y_55, y_56, y_57, y_58, y_59, y_60, y_61, y_62, y_63, y_64, y_65, y_66, y_67, 

X_0, X_1, X_2, X_3, X_4, X_5, X_6, X_7, X_8, X_9, X_10, X_11, X_12, X_13, X_14, X_15, X_16, X_17, X_18, X_19, X_20, X_21, X_22, X_23, X_24, X_25, X_26, X_27, X_28, X_29, X_30, 
X_31, X_32, X_33, X_34, X_35, X_36, X_37, X_38, X_39, X_40, X_41, X_42, X_43, X_44, X_45, X_46, X_47, X_48, X_49, X_50, X_51, X_52, X_53, X_54, X_55, X_56, X_57, X_58, X_59, X_60, 
X_61, X_62, X_63, X_64, X_65, X_66, X_67, 

Y_0, Y_1, Y_2, Y_3, Y_4, Y_5, Y_6, Y_7, Y_8, Y_9, Y_10, Y_11, Y_12, Y_13, Y_14, Y_15, Y_16, Y_17, Y_18, Y_19, Y_20, Y_21, Y_22, Y_23, Y_24, Y_25, Y_26, Y_27, Y_28, Y_29, Y_30, 
Y_31, Y_32, Y_33, Y_34, Y_35, Y_36, Y_37, Y_38, Y_39, Y_40, Y_41, Y_42, Y_43, Y_44, Y_45, Y_46, Y_47, Y_48, Y_49, Y_50, Y_51, Y_52, Y_53, Y_54, Y_55, Y_56, Y_57, Y_58, Y_59, Y_60, 
Y_61, Y_62, Y_63, Y_64, Y_65, Y_66, Y_67, 

Z_0, Z_1, Z_2, Z_3, Z_4, Z_5, Z_6, Z_7, Z_8, Z_9, Z_10, Z_11, Z_12, Z_13, Z_14, Z_15, Z_16, Z_17, Z_18, Z_19, Z_20, Z_21, Z_22, Z_23, Z_24, Z_25, Z_26, Z_27, Z_28, Z_29, Z_30, 
Z_31, Z_32, Z_33, Z_34, Z_35, Z_36, Z_37, Z_38, Z_39, Z_40, Z_41, Z_42, Z_43, Z_44, Z_45, Z_46, Z_47, Z_48, Z_49, Z_50, Z_51, Z_52, Z_53, Z_54, Z_55, Z_56, Z_57, Z_58, Z_59, Z_60, 
Z_61, Z_62, Z_63, Z_64, Z_65, Z_66,Z_67, 

p_scale, p_rx, p_ry, p_rz, p_tx, p_ty, 

p_0, p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10, p_11, p_12, p_13, p_14, p_15, p_16, p_17, p_18, p_19, p_20, p_21, p_22, p_23, p_24, p_25, p_26, p_27, p_28, p_29, p_30, p_31, p_32, p_33, 

AU01_r, AU02_r, AU04_r, AU05_r, AU06_r, AU07_r, AU09_r, AU10_r, AU12_r, AU14_r, AU15_r, AU17_r, AU20_r, AU23_r, AU25_r, AU26_r, AU45_r, 
AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, AU26_c, AU28_c, AU45_c

```


## Basic

* `frame/face` the number of the frame (in case of sequences) and the face (in case of individual images) processed
* `timestamp` the timer of video being processed in seconds
* `confidence` how confident is the tracker in current landmark detection estimage
* `success` is the track successful (is there a face in the frame or do we think we tracked it well)



## Gaze related

* `gaze_0_x, gaze_0_y, gaze_0_z` Eye gaze direction vector in world coordinates for eye 0 (normalized), eye 0 is the leftmost eye in the image
* `gaze_1_x, gaze_1_y, gaze_1_z` Eye gaze direction vector in world coordinates for eye 1 (normalized), eye 1 is the rightmost eye in the image


## Pose

* `pose_Tx, pose_Ty, pose_Tz` the location of the head with respect to camera in millimetre (positive Z is away from the camera)
* `pose_Rx, pose_Ry, pose_Rz` Rotation is in radians around X,Y,Z axes with the convention R = Rx * Ry * Rz, left-handed positive sign. 
This can be seen as pitch (Rx), yaw (Ry), and roll (Rz). The rotation is in world coordinates with camera being the origin.


## Landmarks locations in 2D

* `x_0, x_1, ... x_66, x_67, y_0,...y_67` location of 2D landmarks in pixels, the landmark index can be seen below

## Landmarks locations in 3D

* `X_0, ... X_67, Y_0,...Y_67, Z_0,...Z_67` location of 3D landmarks in millimetres, the landmark index can be seen below.

![Landmarks locations](https://github.com/TadasBaltrusaitis/OpenFace/raw/master/imgs/landmark_scheme_68.png)




## Rigid and non-rigid shape parameters

Parameters of a point distribution model (PDM) that describe the rigid face shape (location, scale and rotation) 
and non-rigid face shape (deformation due to expression and identity). 

* `p_scale, p_rx, p_ry, p_rz, p_tx, p_ty` - scale, rotation and translation terms of the PDM
* `p_0, p_1, ... p_33` - non-rigid shape parameters


## Facial Action Units

Facial Action Units (AUs) are a way to describe human facial expression, 
more details on Action Units can be found here

The system can detect the intensity (from 0 to 5) of 17 AUs:
`AU01_r, AU02_r, AU04_r, AU05_r, AU06_r, AU07_r, AU09_r, AU10_r, AU12_r, AU14_r, AU15_r, AU17_r, AU20_r, AU23_r, AU25_r, AU26_r, AU45_r`

And the presense (0 absent, 1 present) of 18 AUs:
`AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, AU26_c, AU28_c, AU45_c`








## MORE INFORMATION 

For more details look at chapter 4.2 of [Baltrusatis's Thesis](https://www.cl.cam.ac.uk/~tb346/pub/thesis/phd_thesis.pdf) or the 
[wiki of OpenFace for output formal](https://github.com/TadasBaltrusaitis/OpenFace/wiki/Output-Format).







