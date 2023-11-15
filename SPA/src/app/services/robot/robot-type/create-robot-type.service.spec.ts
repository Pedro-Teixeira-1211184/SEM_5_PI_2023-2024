import { TestBed } from '@angular/core/testing';

import { CreateRobotTypeService } from './create-robot-type.service';

describe('CreateRobotTypeService', () => {
  let service: CreateRobotTypeService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(CreateRobotTypeService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
