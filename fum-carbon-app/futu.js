futu = (function () {
  console.info("loading futu");

  function $(selector, el) {
    el = el || document;
    return el.querySelector(selector, el);
  }

  // mandatory element
  function $_(selector, el) {
    el = el || document;
    res = el.querySelector(selector, el);
    assert(res, "Non-existing element for selector: " + selector);
    return res;
  }

  function $$(selector, el) {
    el = el || document;
    var res = el.querySelectorAll(selector, el);
    return Array.prototype.slice.call(res);
  }

  function assert(cond, msg) {
    if (!cond) {
      console.error(msg);
      throw new Error(msg);
    }
  }

  function traceCall(f, args) {
    var args = [f.name].concat(_.toArray(args));
    console.info.apply(console, args);
  }

  function buttonOnClick(btn, callback) {
    btn.addEventListener("click", function (e) {
      e.preventDefault();
      callback(e);
      return false;
    });
  }

  // global initialisation
  
  var onloadCallbacks = []; 

  function onload(f) {
    onloadCallbacks.push(f);
  }

  function linkButton(btn) {
    btn.disabled = false;
    buttonOnClick(btn, function () {
      location.href = btn.dataset.futuLinkButton;
    });
  }

  document.addEventListener("DOMContentLoaded", function () {
    console.info("initialising futu");

    // use jQuery datepickers
    $$("input[type=date]").forEach(function (el) {
      el.type = "text";
      jQuery(el). datepicker({
        dateFormat: "yy-mm-dd",
        onSelect: function (dateText, inst) {
          this.value = dateText;
          this.dispatchEvent(new Event("change"));
        },
      });
    });

    // Link buttons
    $$("button[data-futu-link-button]").forEach(linkButton);

    // registered callbacks
    onloadCallbacks.forEach(function (f) {
      f();
    });
  });

  // exports

  return {
    $: $,
    $_: $_,
    $$: $$,
    assert: assert,
    onload: onload,
    buttonOnClick: buttonOnClick,
  };
}());
