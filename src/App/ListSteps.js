"use strict";

export const _getNewSteps = (currLenghtRead) => async () => {
    const stepStrings = await fetch(`http://127.0.0.1:7379/LRANGE/steps_json/${currLenghtRead}/-1`)
        .then((resp) => resp.json())
        .then((json) => json["LRANGE"])
    return stepStrings;
}