leaflet() %>% addTiles() %>%
  addMarkers(data=quakes,
    clusterOptions = markerClusterOptions(),
    clusterId = "pClust") %>%
  addEasyButton(easyButton(
    states = list(
      easyButtonState(
        stateName="clustered-markers",
        icon="ion-toggle",
        title="individual",
        onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'pClust');
            clusterManager.disableClustering();
            btn.state('indvidual-markers');
          }")
      ),
      easyButtonState(
        stateName="indvidual-markers",
        icon="ion-toggle-filled",
        title="cluster",
        onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'pClust');
            clusterManager.enableClustering();
            btn.state('clustered-markers');
          }")
      )
    )
  ))

