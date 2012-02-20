% This is proposal of model structure definition
-module(single_car).
-export([describe/1]).


describe(root) ->
    [
        % driver description is in the this module
        {child, driver},
        % ... but car is defined separately
        {child, car, car_definition},
        {child, road},

        % Define links between slots
        {connect, driver_control, {driver, control}},
        {connect, {driver, eyes}, {car, gauge}},

        % Yes, it is bad way of controlling car
        {connect, {driver,  left_leg}, {car, brake}},
        {connect, {driver, right_leg}, {car, accelerate}},

        {connect, {car, wheels}, {road, surface}}
    ];

describe(driver) ->
    [
        {implementation, stupid_driver}
    ];

describe(road) ->
    [
        {implementation, simple_road}
    ].
