import React from "react";
import createReactClass from "create-react-class";

import logViewer from "./logViewer";

export default createReactClass({
  initialize(node) {
    if (node === null) return;

    // Elm 0.19
    // Note that Elm 0.19 doesn't treat `node` as a container, but rather as a placeholder.
    // The Elm App will replace the provided node, rather than mount itself within the provided node.
    // This can cause react-dom runtime errors when unmounting this React component.
    // The workaround is to create an extra <div>, which React doesn't control, and allow Elm to replace that node.
    const elmPlaceholder = document.createElement("div");
    node.appendChild(elmPlaceholder);
    const app = logViewer.init({
      node: elmPlaceholder,
      flags: this.props.flags
    });

    if (app && typeof this.props.ports !== "undefined") {
      this.props.ports(app.ports);
    }
  },

  shouldComponentUpdate() {
    return false;
  },

  render() {
    return React.createElement("div", { ref: this.initialize });
  }
});
