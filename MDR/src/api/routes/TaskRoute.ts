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

  route.get('/types', (req, res, next) => {
    console.log("Getting all task types!");
    ctrl.getAllTaskTypes(req, res, next);
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

  route.get('/', (req, res, next) => {
    console.log("Getting all tasks!");
    const tasks = [
      { inicialTask: '76f0bfc4-255a-4c31-a4ee-0cc553cf258f', nextTask: '9a122825-2e91-4ee8-b785-bffed7d4253a', time: 1078 },
      { inicialTask: '9a122825-2e91-4ee8-b785-bffed7d4253a', nextTask: 'e924cb8d-0c99-416d-8bfe-2e3e12aa2660', time: 2000 },
      { inicialTask: 'e924cb8d-0c99-416d-8bfe-2e3e12aa2660', nextTask: 'e924cb8d-0c99-416d-8bfe-2e3e12aa2660', time: 3500 },
      // Add more tasks as needed
  ];
  const formattedTasks = tasks.map(task => `${task.inicialTask},${task.nextTask},${task.time}`);
  res.json(tasks);
  });

  return route;
}
