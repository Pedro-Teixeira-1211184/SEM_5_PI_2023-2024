import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TaskRequestUpdateComponent } from './task-request-update.component';

describe('TaskRequestUpdateComponent', () => {
  let component: TaskRequestUpdateComponent;
  let fixture: ComponentFixture<TaskRequestUpdateComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [TaskRequestUpdateComponent]
    });
    fixture = TestBed.createComponent(TaskRequestUpdateComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
