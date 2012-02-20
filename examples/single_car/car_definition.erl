-module(car_definition).
-export([describe/1]).

describe(root) -> describe(car);
describe(car) ->
    [
        {child, engine},
        {child, transmission},
        {child, brakes},
        {child, wheels},
        {child, pedals},
        {child, physics},
        {child, gauge},

        {connect, accelerate, {pedals, accelerate}},
        {connect, brake, {pedals, brake},
        {connect, {gauge, display}, gauge},
        {connect, {physics, report}, reports},

        {connect, {pedals, fuel_pipe}, {engine, fuel}},
        {connect, {pedals, brake_pipe}, {brake, action}},

        {connect, {engine, shaft}, {transmission, engine}},
        {connect, {transmission, wheels}, {wheels, drive}},
        {connect, {wheels, road} wheels},

        {connect, {wheels, force}, {physics, force}},
        % ......
    ].

