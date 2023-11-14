import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DeactivateRobotComponent } from './deactivate-robot.component';

describe('DeactivateRobotComponent', () => {
  let component: DeactivateRobotComponent;
  let fixture: ComponentFixture<DeactivateRobotComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [DeactivateRobotComponent]
    });
    fixture = TestBed.createComponent(DeactivateRobotComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
