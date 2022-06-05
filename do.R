library(leaflet)
## 数据源
stations <- read.csv("stations.csv", stringsAsFactors=TRUE)
lines_color <- read.csv("lines_color.csv")
##在地图上标点
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=116.294, lat=39.9742, popup="巴沟")
m  # Print the map

##设置底层画布-北京市
beijing <- leaflet() %>%
  setView(lng=116.35,lat=39.90,zoom = 11) %>% addProviderTiles("CartoDB.Positron")
beijing

##图例颜色
pal <- colorFactor(lines_color$color, domain = stations$line)

##绘制地图辅助函数
draw_line_add <- function(l_no,line_s_id=NULL){
  line_color <- lines_color[lines_color$line==l_no,]$color
  line_data <- stations[stations$line==l_no,]
  if(is.null(line_s_id)){
    draw_lines <- beijing %>%
      addPolylines(lat=line_data$s_lat,lng=line_data$sl_lng,color=line_color,weight = 2,smoothFactor = 1,fillOpacity = 0)
  }else{
    draw_lines <- beijing %>%
      addPolylines(lat=line_data$s_lat[line_s_id],lng=line_data$sl_lng[line_s_id],color=line_color,weight = 2,smoothFactor = 1,fillOpacity = 0)
  }
  return(draw_lines)
}
##绘制地铁线路图

for(l in unique(stations$line)){
  line_length <- nrow(stations[stations$line==l,])
  beijing <- draw_line_add(l_no=l)
}
##换乘图标
icon <- makeIcon(
  iconUrl = "北京换乘.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 0, iconAnchorY = 0
  
)
icon2 <- makeIcon(
  iconUrl = "未选中圆圈.png",
  iconWidth = 7, iconHeight = 7,
  iconAnchorX = 0, iconAnchorY = 0
  
)

##添加地铁站名称图例，站点标签，换乘图标

beijing<- beijing%>%
  addCircleMarkers(stations$sl_lng, stations$s_lat, popup =paste(stations$n,stations$line,sep=","),color = 'white', radius=0.01,fillOpacity = 0) %>%
  addLegend(pal=pal,values = stations$line) %>%
  addLabelOnlyMarkers(
    lng = stations$sl_lng, lat = stations$s_lat,
    label = stations$n,
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>%
  addMarkers(stations[stations$t==1,]$sl_lng, lat = stations[stations$t==1,]$s_lat, icon = icon)%>%
  addMarkers(stations[stations$t==0,]$sl_lng, lat = stations[stations$t==0,]$s_lat, icon = icon2)
beijing



