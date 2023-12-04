import {Router} from 'express';
import {celebrate, Joi} from 'celebrate';

import {Container} from 'typedi';
import config from "../../../config";
import ITaskController from "../../controllers/IControllers/ITaskController";

const route = Router();

export default () => {

  const ctrl = Container.get(config.controllers.task.name) as ITaskController;

  route.post('/types', celebrate({
    body: Joi.object({
      designation: Joi.string().required().error(new Error('Invalid designation')),
      robotType: Joi.string().required().error(new Error('Invalid robot type')),
    })
  }), (req, res, next) => {
    console.log("Creating a task type!");
    ctrl.createTaskType(req, res, next);
  });

  route.post('/', celebrate({
    body: Joi.object({
      username: Joi.string().required().error(new Error('Invalid user')),
      email: Joi.string().required().error(new Error('Invalid email')),
      other_contact: Joi.string().error(new Error('Invalid other contact')),
      taskType: Joi.string().required().error(new Error('Invalid task type')),
      target: Joi.string().required().error(new Error('Invalid target')),
      time: Joi.date().required().error(new Error('Invalid time')),
    })
  }), (req, res, next) => {
    console.log("Creating a task!");
    ctrl.createTaskRequest(req, res, next);
  });

  return route;
}
