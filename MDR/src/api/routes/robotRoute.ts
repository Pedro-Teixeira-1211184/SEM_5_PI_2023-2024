import {Router} from 'express';
import {celebrate, Joi} from 'celebrate';

import {Container} from 'typedi';

import config from "../../../config";
import IRobotController from "../../controllers/IControllers/IRobotController";

const route = Router();

export default () => {

    const ctrl = Container.get(config.controllers.robot.name) as IRobotController;

    route.post('/', celebrate({
        body: Joi.object({
            robotType: Joi.string().required().max(25).error(new Error('robotType is required!')),
            code: Joi.string().required().max(100).error(new Error('Invalid robot code')),
            serialNumber: Joi.string().required().error(new Error('Invalid robot serialNumber')),
            nickname: Joi.string().required().error(new Error('Invalid robot nickname')),
            brand: Joi.string().required().max(50).error(new Error('Invalid robot brand')),
            isActive: Joi.boolean().error(new Error('Invalid robot status'))
        })
    }), (req, res, next) => {
        console.log("Creating a robot!");
        ctrl.createRobot(req, res, next);
    });

    route.post('/types', celebrate({
            body: Joi.object({
                designation: Joi.string().required().max(25).error(new Error('robotType is required!')),
            })
        }), (req, res, next) => {
            console.log("Creating a robot type!");
            ctrl.createRobotType(req, res, next);
        }
    );

    route.get('/types', (req, res, next) => {
        console.log("Creating a robot type!");
        ctrl.getRobotType(req, res, next);
      }
    );

    route.patch('/:robot_id', celebrate({
        body: Joi.object({
            isActive: Joi.boolean().error(new Error('Invalid robot status'))
        })
    }), (req, res, next) => {
        console.log("Updating a robot!");
        ctrl.updateRobot(req, res, next);
    });

    route.get('/', (req, res, next) => {
        console.log("Getting all robots!");
        ctrl.getAllRobots(req, res, next);
    });

    route.get('/:robot_code', (req, res, next) => {
        console.log("Getting a robot!");
        ctrl.getRobotByCode(req, res, next);
    });

    return route;
}
