"use strict";

export const _scrollToElementIdIfNotVisible = (elementId) => async () => {
  const elem = document.getElementById(elementId)
  if(elem){
    elem.scrollIntoViewIfNeeded()
  }
}