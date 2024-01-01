import { ComponentFixture, TestBed } from '@angular/core/testing';

import { NotApprovedTaskComponent } from './not-approved-task.component';

describe('NotApprovedTaskComponent', () => {
  let component: NotApprovedTaskComponent;
  let fixture: ComponentFixture<NotApprovedTaskComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [NotApprovedTaskComponent]
    });
    fixture = TestBed.createComponent(NotApprovedTaskComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
