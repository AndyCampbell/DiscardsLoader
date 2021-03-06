

/****** Script for SelectTopNRows command from SSMS  ******/

use [FEAS_DemDiscards]
go


  SELECT *
  FROM [FEAS_DemDiscards].[dbo].[CRUISe]
  WHERE [Cruise Code] = 'FAT/CTB/17/5'

  DELETE
  FROM [FEAS_DemDiscards].[dbo].[CRUISe]
  WHERE [Cruise Code] = 'FAT/CTB/17/5'

  SELECT *
  FROM [FEAS_DemDiscards].[dbo].[HAUl]
  WHERE [Cruise Code] = 'FAT/CTB/17/5'

  DELETE
  FROM [FEAS_DemDiscards].[dbo].[HAUl]
  WHERE [Cruise Code] = 'FAT/CTB/17/5'

  SELECT *
  FROM [FEAS_DemDiscards].[dbo].[BULk]
  WHERE [Cruise Code] = 'FAT/CTB/17/5'

  DELETE
  FROM [FEAS_DemDiscards].[dbo].[BULk]
  WHERE [Cruise Code] = 'FAT/CTB/17/5'

  SELECT *
  FROM [FEAS_DemDiscards].[dbo].[SAMPLE-HEADEr]
  WHERE [Cruise Code] = 'FAT/CTB/17/5'

  DELETE
  FROM [FEAS_DemDiscards].[dbo].[SAMPLE-HEADEr]
  WHERE [Cruise Code] = 'FAT/CTB/17/5'

  SELECT *
  FROM [FEAS_DemDiscards].[dbo].[SAMPLe]
  WHERE [sample_header_id] IN
  (SELECT sample_header_id FROM [FEAS_DemDiscards].[dbo].[SAMPLE-HEADEr] WHERE [Cruise code] = 'FAT/CTB/17/5')

  DELETE
  FROM [FEAS_DemDiscards].[dbo].[SAMPLe]
  WHERE [sample_header_id] IN
  (SELECT sample_header_id FROM [FEAS_DemDiscards].[dbo].[SAMPLE-HEADEr] WHERE [Cruise code] = 'FAT/CTB/17/5')
