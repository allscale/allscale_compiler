"use strict";

function determineLevelForEntry(entry) {
	var level = 'success';
	for (var i = 0; i < entry.issues.length; i++) {
		var issue = entry.issues[i];
		if (issue.severity == 'Warning') {
			level = 'warning';
		} else if (issue.severity == 'Error') {
			level = 'danger';
			break;
		}
	}
	return level;
}

function determineLevelForIssue(issue) {
	if (issue.severity == 'Error') {
		return 'danger';
	} else if (issue.severity == 'Warning') {
		return 'warning';
	} else {
		return 'info';
	}
}

function getHelpMessage(error_code) {
	if (error_code in report.help_messages) {
		return report.help_messages[error_code];
	} else {
		return ""
	}
}

function setProgress() {
	var noWarnings = true;
	var success = 0;
	var count = Object.keys(report.conversions).length;
	for (var addr in report.conversions) {
		var entry = report.conversions[addr];
		var level = determineLevelForEntry(entry);
		if (level != 'danger') {
			success++;
		}
		if (level == 'warning') {
			noWarnings = false;
		}
	}

	$('#progress-num').text(`${success} / ${count}`);

	var bar = $('#progress .progress-bar');

	if (success == count && count > 0 && noWarnings) {
		bar.addClass('progress-bar-success');
	} else if (success == count && count > 0) {
		bar.addClass('progress-bar-warning');
	} else if (count > 0) {
		bar.addClass('progress-bar-danger');
	}

	var percent = 100.0;
	if (count > 0) {
		percent = 100.0 * (success / count);
	}
	bar.attr('style', `width: ${percent}%`)
}

function createSource(addr, location, source, ommitLocation) {

	var firstLine = parseInt(location.substr(location.indexOf('@') + 1));

	var $location = $('<div>').addClass('location').append(
		$('<div>').addClass('nodeaddress').text(addr)
	);

	if (!ommitLocation) {
		$location.append(
			$('<div>').addClass('filename').text(location)
		);
	}

	return $('<pre>')
		.attr('data-start', firstLine)
		.append(
			$location,
			$('<code>').addClass('language-cpp line-numbers').append(source)
		);
}

function createBacktrace(addr, issue_index, issue, trace, index) {
	var id = `entry-${addr}-issue-${issue_index}-backtrace-${index}`;

	var title = trace.address;

	if (trace.location) {
		title = trace.location;
	}

	var $body = $('<div>').addClass('panel-body');

	if (trace.source) {
		$body.append(createSource(addr, trace.location, trace.source, true));
	}

	if (!$body.html()) {
		$body.html("<i>No location information.</i>");
	}

	return $('<div>')
		.addClass('panel panel-default')
		.addClass(!trace.location ? 'internal' : '')
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<a>')
						.attr('href', `#${id}`)
						.attr('data-toggle', 'collapse')
						.attr('data-parent', `#entry-${addr}-issue-${issue_index}-backtrace`)
						.text(title)
				),
			$('<div>')
				.attr('id', id)
				.addClass('panel-collapse collapse')
				.append($body)
		);
}

function createTags(issue) {
	if (!issue.tags) {
		return;
	}
	return $.map(issue.tags, function(tag) {
		return $('<div>').addClass('label label-default').text(tag);
	});
}

function createIssue(addr, issue, index) {
	return $('<div>')
		.addClass(`entry-issue panel panel-${determineLevelForIssue(issue)}`)
		.addClass(
			$.map(issue.tags, function(tag) {
				return `tag-${tag}`;
			}).join(' ')
		)
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<div>')
						.addClass('entry-issue-error-code')
						.text(issue.error_code),
					$('<div>')
						.addClass('entry-issue-message')
						.append(
							$('<a>')
								.attr('href', `#entry-${addr}-issue-${index}`)
								.attr('data-toggle', 'collapse')
								.addClass('panel-title')
								.html(`<strong>${issue.severity}:</strong> ${issue.message}`)
						),
					$('<div>')
						.addClass('entry-issue-tags')
						.append(createTags(issue))
				),
			$('<div>')
				.attr('id', `entry-${addr}-issue-${index}`)
				.addClass('entry-issue-body collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$('<pre>')
								.addClass('entry-issue-detail')
								.text(issue.detail ? issue.detail : ''),
							$('<div>')
								.addClass('entry-issue-help')
								.text(getHelpMessage(issue.error_code)),
							createSource(addr, issue.loc.location, issue.loc.source),
							$('<div>')
								.attr('id', `entry-${addr}-issue-${index}-backtrace`)
								.addClass('panel-group')
								.addClass('backtrace')
								.append(
									$('<div>').addClass('backtrace-text').text('Backtrace'),
									$.map(issue.backtrace, $.proxy(createBacktrace, null, addr, index, issue))
								)
						)
				)
		);
}

function createConversion(entry, addr) {
	var loc = entry.loc_user ? entry.loc_user : entry.loc;

	return $('<div>')
		.addClass(`entry panel panel-${determineLevelForEntry(entry)}`)
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<i>').addClass('glyphicon glyphicon-file'),
					' ',
					$('<a>')
						.attr('href', `#entry-${addr}`)
						.attr('data-toggle', 'collapse')
						.addClass('panel-title')
						.text(loc.location)
				),
			$('<div>')
				.attr('id', `entry-${addr}`)
				.addClass('collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$.map(entry.issues, $.proxy(createIssue, null, addr)),
							createSource(addr, loc.location, loc.source)
						)
				)
		);
}

function createRaw() {
	return $('<div>')
		.addClass('panel panel-default internal')
		.append(
			$('<div>')
				.addClass('panel-heading')
				.attr('style', 'text-align: center')
				.append(
					$('<a>')
						.attr('href', '#raw')
						.attr('data-toggle', 'collapse')
						.text('Raw Conversion Report')
				),
			$('<div>')
				.attr('id', 'raw')
				.addClass('collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$('<pre>').text(JSON.stringify(report, null, 2))
						)
				)
		);
}

function setupControls() {
	// filter by tag
	{
		// collect all tags
		var tags = new Set();
		for (var addr in report.conversions) {
			var conversion = report.conversions[addr];
			for (var index = 0; index < conversion.issues.length; index++) {
				var issue = conversion.issues[index];
				if (issue.tags) {
					tags = new Set([...tags, ...issue.tags]);
				}
			}
		}
		tags = Array.from(tags);

		if (tags.length > 0) {
			$('#filters').append(
				$('<h4>').text('Filter Tags'),
				$.map(tags, function(tag) {
					return $('<div>')
						.addClass('checkbox')
						.append($('<label>').append(
							$('<input>')
								.attr('type', 'checkbox')
								.attr('checked', true)
								.change(function() {
									if (this.checked) {
										$(`.tag-${tag}`).show();
									} else {
										$(`.tag-${tag}`).hide();
									}
								}),
							tag
						))
				})
			);
		}

	}

	// internal
	{
		var $internals = $('.internal').add('.nodeaddress');

		$internals.hide();

		$('#internal-button').click(function() {
			$internals.toggle();
		});
	}
}

function main() {
	$('#main').append($.map(report.conversions, createConversion));

	// open if only 1
	if (Object.keys(report.conversions).length == 1) {
		for (var addr in report.conversions) {
			$(`#entry-${addr}`).collapse('show');
		}
	}

	// add raw block
	$('#main').append(createRaw());

	setProgress();
	setupControls();
}

main();
