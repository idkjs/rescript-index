// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "@rescript/std/lib/es6/curry.js";
import * as React from "react";
import * as UsePrevious from "../src/UsePrevious.js";

function $$default(param) {
  var match = React.useState(function () {
        return 0;
      });
  var setValue = match[1];
  var value = match[0];
  var previousValue = UsePrevious.usePrevious(value);
  React.useEffect((function () {
          setInterval((function (param) {
                  return Curry._1(setValue, (function (prev) {
                                return prev + 1 | 0;
                              }));
                }), 1000);
          
        }), []);
  return React.createElement("div", {
              className: "p-4"
            }, React.createElement("div", undefined, "Previous Value: ", String(previousValue)), React.createElement("div", undefined, "Value: ", String(value)));
}

export {
  $$default ,
  $$default as default,
  
}
/* react Not a pure module */
