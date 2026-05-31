(function () {
  var exportScale = 2;

  function canPatchEasyPrint() {
    return window.L &&
      window.L.Control &&
      window.L.Control.EasyPrint &&
      window.domtoimage &&
      window.saveAs;
  }

  function restorePlugin(plugin) {
    plugin._toggleControls(true);

    if (plugin._toggleClasses && plugin.options.hideClasses) {
      plugin._toggleClasses(plugin.options.hideClasses, true);
    }

    if (plugin.outerContainer) {
      if (plugin.originalState.widthWasAuto) {
        plugin.mapContainer.style.width = "auto";
      } else if (plugin.originalState.widthWasPercentage) {
        plugin.mapContainer.style.width = plugin.originalState.percentageWidth;
      } else {
        plugin.mapContainer.style.width = plugin.originalState.mapWidth;
      }

      plugin.mapContainer.style.height = plugin.originalState.mapHeight;
      plugin._removeOuterContainer(
        plugin.mapContainer,
        plugin.outerContainer,
        plugin.blankDiv
      );
      plugin._map.invalidateSize();
      plugin._map.setView(plugin.originalState.center);
      plugin._map.setZoom(plugin.originalState.zoom);
    }
  }

  function patchEasyPrint() {
    if (!canPatchEasyPrint() || window.L.Control.EasyPrint.prototype._highResPatched) {
      return;
    }

    var originalPrintOperation = window.L.Control.EasyPrint.prototype._printOpertion;

    window.L.Control.EasyPrint.prototype._printOpertion = function (sizeMode) {
      var plugin = this;
      var widthForExport = this.mapContainer.style.width;
      var heightForExport = this.mapContainer.style.height;
      var width;
      var height;

      if (sizeMode !== "CurrentSize") {
        return originalPrintOperation.call(this, sizeMode);
      }

      if (this.originalState.widthWasAuto || this.originalState.widthWasPercentage) {
        widthForExport = this.originalState.mapWidth;
      }

      width = parseInt(widthForExport, 10) || this._map.getSize().x;
      height = parseInt(heightForExport, 10) || this._map.getSize().y;

      window.domtoimage.toPng(plugin.mapContainer, {
        width: width * exportScale,
        height: height * exportScale,
        style: {
          transform: "scale(" + exportScale + ")",
          transformOrigin: "top left",
          width: width + "px",
          height: height + "px"
        }
      })
        .then(function (dataUrl) {
          var blob = plugin._dataURItoBlob(dataUrl);

          if (plugin.options.exportOnly) {
            window.saveAs(blob, plugin.options.filename + ".png");
          } else {
            plugin._sendToBrowserPrint(dataUrl, plugin.orientation);
          }

          restorePlugin(plugin);
          plugin._map.fire("easyPrint-finished");
        })
        .catch(function (error) {
          console.error("Print operation failed", error);
          restorePlugin(plugin);
        });
    };

    window.L.Control.EasyPrint.prototype._highResPatched = true;
  }

  patchEasyPrint();
  document.addEventListener("DOMContentLoaded", patchEasyPrint);
  window.setTimeout(patchEasyPrint, 500);
  window.setTimeout(patchEasyPrint, 1500);
  window.setInterval(patchEasyPrint, 1000);
}());
