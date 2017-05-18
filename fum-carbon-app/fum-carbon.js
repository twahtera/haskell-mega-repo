document.addEventListener("DOMContentLoaded", function () {
  console.info("Initialising fum-carbon js");

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

  function linkButton(btn) {
    btn.disabled = false;
    buttonOnClick(btn, function () {
      location.href = btn.dataset.futuLinkButton;
    });
  }

  // Utilities

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

  function inputValue(el) {
    if (el.tagName === "INPUT" && el.type === "checkbox") {
        return el.checked;
    } else if (el.tagName === "INPUT" || el.tagName === "TEXTAREA") {
        return el.value.trim();
    } else if (el.tagName === "SELECT" && el.multiple) {
        return $$("option:checked", el).map(function (o) { return o.value.trim(); });
    } else if (el.tagName === "SELECT") {
        return el.value.trim();
    } else {
        throw new Error("inputValue: how to handle " + el.tagName);
    }
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
});
