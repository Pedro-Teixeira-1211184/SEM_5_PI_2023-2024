import {Router} from 'express';
import {celebrate, Joi} from 'celebrate';

import {Container} from 'typedi';
import IPassagewayController from '../../controllers/IControllers/IPassagewayController';

import config from "../../../config";

const route = Router();

export default () => {

    const ctrl = Container.get(config.controllers.passageway.name) as IPassagewayController;

    route.post('/', celebrate({
        body: Joi.object({
            floorID1: Joi.string().required().error(new Error('Invalid floorID code')),
            floorID2: Joi.string().required().error(new Error('Invalid floorID code')),
            localization1: Joi.string().regex(new RegExp('^[(][0-9]+[,][0-9]+[)][;][(][0-9]+[,][0-9]+[)]$')).required().error(new Error('Invalid localization1')),
            localization2: Joi.string().regex(new RegExp('^[(][0-9]+[,][0-9]+[)][;][(][0-9]+[,][0-9]+[)]$')).required().error(new Error('Invalid localization2'))
        })
    }), (req, res, next) => {
        console.log("Creating a passageway!");
        ctrl.createPassageway(req, res, next);
    });

    return route;
}
